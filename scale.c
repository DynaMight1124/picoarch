#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "main.h"
#include "options.h"
#include "scale.h"

int video_width  = 0;
int video_height = 0;

typedef void (*scaler_t)(unsigned w, unsigned h, size_t pitch, const void *src, void *dst);

struct dimensions {
	unsigned w;
	unsigned h;
	size_t pitch;
};

struct blend_args {
	int w_ratio_in;
	int w_ratio_out;
	uint16_t w_bp[2];
	int h_ratio_in;
	int h_ratio_out;
	uint16_t h_bp[2];
	uint16_t *blend_line;
} blend_args;

static scaler_t scaler;
static scaler_t crop_scaler;
static unsigned dst_w, dst_h, dst_offs, src_offs, w_offs;
struct dimensions prev;

#if __ARM_ARCH >= 5
static inline uint32_t average16(uint32_t c1, uint32_t c2) {
	uint32_t ret, lowbits = 0x0821;
	asm ("eor %0, %2, %3\r\n"
	     "and %0, %0, %1\r\n"
	     "add %0, %3, %0\r\n"
	     "add %0, %0, %2\r\n"
	     "lsr %0, %0, #1\r\n"
	     : "=&r" (ret) : "r" (lowbits), "r" (c1), "r" (c2) : );
	return ret;
}

static inline uint32_t average32(uint32_t c1, uint32_t c2) {
	uint32_t ret, lowbits = 0x08210821;

	asm ("eor %0, %3, %1\r\n"
	     "and %0, %0, %2\r\n"
	     "adds %0, %1, %0\r\n"
	     "and %1, %1, #0\r\n"
	     "movcs %1, #0x80000000\r\n"
	     "adds %0, %0, %3\r\n"
	     "rrx %0, %0\r\n"
	     "orr %0, %0, %1\r\n"
	     : "=&r" (ret), "+r" (c2) : "r" (lowbits), "r" (c1) : "cc" );

	return ret;
}

#define AVERAGE16_NOCHK(c1, c2) (average16((c1), (c2)))
#define AVERAGE32_NOCHK(c1, c2) (average32((c1), (c2)))

#else

static inline uint32_t average16(uint32_t c1, uint32_t c2) {
	return (c1 + c2 + ((c1 ^ c2) & 0x0821))>>1;
}

static inline uint32_t average32(uint32_t c1, uint32_t c2) {
	uint32_t sum = c1 + c2;
	uint32_t ret = sum + ((c1 ^ c2) & 0x08210821);
	uint32_t of = ((sum < c1) | (ret < sum)) << 31;

	return (ret >> 1) | of;
}

#define AVERAGE16_NOCHK(c1, c2) (average16((c1), (c2)))
#define AVERAGE32_NOCHK(c1, c2) (average32((c1), (c2)))

#endif


#define AVERAGE16(c1, c2) ((c1) == (c2) ? (c1) : AVERAGE16_NOCHK((c1), (c2)))
#define AVERAGE16_1_3(c1, c2) ((c1) == (c2) ? (c1) : (AVERAGE16_NOCHK(AVERAGE16_NOCHK((c1), (c2)), (c2))))

#define AVERAGE32(c1, c2) ((c1) == (c2) ? (c1) : AVERAGE32_NOCHK((c1), (c2)))
#define AVERAGE32_1_3(c1, c2) ((c1) == (c2) ? (c1) : (AVERAGE32_NOCHK(AVERAGE32_NOCHK((c1), (c2)), (c2))))

static inline int gcd(int a, int b) {
	return b ? gcd(b, a % b) : a;
}

static void scale_null(unsigned w, unsigned h, size_t pitch, const void *src, void *dst) {}

static void scale_1x(unsigned w, unsigned h, size_t pitch, const void *src, void *dst) {
	dst += dst_offs;

	for (unsigned y = 0; y < h; y++) {
		memcpy(dst + y * SCREEN_PITCH, src + y * pitch, w * SCREEN_BPP);
	}
}

static void scale_crop(unsigned w, unsigned h, size_t pitch, const void *src, void *dst) {
	src += src_offs;
	w += w_offs;

	crop_scaler(w, h, pitch, src, dst);
}

static void scale_nearest(unsigned w, unsigned h, size_t pitch, const void *src, void *dst) {
	int dy = -dst_h;
	unsigned lines = h;
	bool copy = false;
	size_t cpy_w = dst_w * SCREEN_BPP;

	dst += dst_offs;

	while (lines) {
		int dx = -dst_w;
		const uint16_t *psrc16 = src;
		uint16_t *pdst16 = dst;

		if (copy) {
			copy = false;
			memcpy(dst, dst - SCREEN_PITCH, cpy_w);
			dst += SCREEN_PITCH;
			dy += h;
		} else if (dy < 0) {
			int col = w;
			while(col--) {
				while (dx < 0) {
					*pdst16++ = *psrc16;
					dx += w;
				}

				dx -= dst_w;
				psrc16++;
			}

			dst += SCREEN_PITCH;
			dy += h;
		}

		if (dy >= 0) {
			dy -= dst_h;
			src += pitch;
			lines--;
		} else {
			copy = true;
		}
	}

	// --- Fix: ensure last destination line is written ---
	{
		uint8_t *base = (uint8_t *)dst - SCREEN_PITCH; // go back one line
		uint8_t *last = base + SCREEN_PITCH;           // point to final line
		memcpy(last, base, SCREEN_PITCH);
	}
}

/* Generic blend based on % of dest pixel in next src pixel, using
 * rough quintiles: aaaa, aaab, aabb, abbb, bbbb. Quintile breakpoints
 * can be adjusted for sharper or smoother blending. Default 0-20%,
 * 20%-50% (round down), 50%(down)-50%(up), 50%(round up)-80%,
 * 80%-100%. This matches existing scalers */
static void scale_blend(unsigned w, unsigned h, size_t pitch, const void *src, void *dst) {
	int dy = 0;
	int lines = h;

	int rat_w = blend_args.w_ratio_in;
	int rat_dst_w = blend_args.w_ratio_out;
	uint16_t *bw = blend_args.w_bp;

	int rat_h = blend_args.h_ratio_in;
	int rat_dst_h = blend_args.h_ratio_out;
	uint16_t *bh = blend_args.h_bp;

	dst += dst_offs;

	while (lines--) {
		while (dy < rat_dst_h) {
			uint16_t *dst16 = (uint16_t *)dst;
			uint16_t *pblend = (uint16_t *)blend_args.blend_line;
			int col = w;
			int dx = 0;

			uint16_t *pnext = (uint16_t *)(src + pitch);
			if (!lines)
				pnext -= (pitch / sizeof(uint16_t));

			if (dy <= bh[0] && dy + rat_h > (rat_dst_h + rat_dst_h - bh[0])) {
				/* Will miss next line, blend in instead */
				const uint32_t *src32 = (const uint32_t *)src;
				const uint32_t *pnext32 = (const uint32_t *)pnext;
				uint32_t *pblend32 = (uint32_t *)pblend;
				int count = w / 2;

				while(count--) {
					*pblend32++ = AVERAGE32(*src32, *pnext32);
					src32++;
					pnext32++;
				}
			} else if (dy > rat_dst_h - bh[0]) {
				pblend = pnext;
			} else if (dy <= bh[0]) {
				/* Drops const, won't get touched later though */
				pblend = (uint16_t *)src;
			} else {
				const uint32_t *src32 = (const uint32_t *)src;
				const uint32_t *pnext32 = (const uint32_t *)pnext;
				uint32_t *pblend32 = (uint32_t *)pblend;
				int count = w / 2;

				if (dy <= bh[1]) {
					const uint32_t *tmp = pnext32;
					pnext32 = src32;
					src32 = tmp;
				}

				if (dy > rat_dst_h - bh[1] || dy <= bh[1]) {
					while(count--) {
						*pblend32++ = AVERAGE32_1_3(*src32, *pnext32);
						src32++;
						pnext32++;
					}
				} else {
					while(count--) {
						*pblend32++ = AVERAGE32(*src32, *pnext32);
						src32++;
						pnext32++;
					}
				}
			}

			while (col--) {
				uint16_t a, b, out;

				a = *pblend;
				b = *(pblend+1);

				while (dx < rat_dst_w) {
					if (a == b) {
						out = a;
					} else if (dx <= bw[0] && dx + rat_w > (rat_dst_w + rat_dst_w - bw[0])) {
						out = AVERAGE16_NOCHK(a, b); // will miss next pixel, blend in instead
					} else if (dx > rat_dst_w - bw[0]) { // top quintile, bbbb
						out = b;
					} else if (dx <= bw[0]) { // last quintile, aaaa
						out = a;
					} else {
						if (dx > rat_dst_w - bw[1]) { // 2nd quintile, abbb
							a = AVERAGE16_NOCHK(a, b);
						} else if (dx <= bw[1]) { // 4th quintile, aaab
							b = AVERAGE16_NOCHK(a, b);
						}

						out = AVERAGE16_NOCHK(a, b); // also 3rd quintile, aabb
					}
					*dst16++ = out;
					dx += rat_w;
				}

				dx -= rat_dst_w;
				pblend++;
			}

			dy += rat_h;
			dst += SCREEN_PITCH;
		}

		dy -= rat_dst_h;
		src += pitch;
	}

	// --- Fix: ensure last destination line is written ---
	{
		uint8_t *base = (uint8_t *)dst - SCREEN_PITCH; // go back one line
		uint8_t *last = base + SCREEN_PITCH;           // point to final line
		memcpy(last, base, SCREEN_PITCH);
	}
}

#define DARKER(c1, c2) (c1 > c2 ? c2 : c1)

// GB 160x144 to 240x216 (40,12) via eggs
static void scale_sharp_160x144_240x216(unsigned _w, unsigned _h, size_t pitch, const void *src_bytes, void *dst_bytes) {
	register uint_fast16_t a,b,c,d,e,f;
	uint32_t x,y;
	uint16_t *src = (uint16_t *)src_bytes;
	uint16_t *dst = (uint16_t *)dst_bytes;
	size_t pitch16 = pitch / sizeof(uint16_t);

	dst += dst_offs / sizeof(uint16_t);

	for (y=(144/2); y>0 ; y--, src+=(pitch16*2 - 160), dst+=(SCREEN_WIDTH*3 - 240))
	{
		for (x=(160/4); x>0; x--, src+=4, dst+=6)
		{
			a = *(src+0);
			b = *(src+1);
			c = *(src+pitch16);
			d = *(src+pitch16+1);
			e = DARKER(a,c);
			f = DARKER(b,d);

			*(uint32_t*)(dst+             0) = a|(DARKER(a,b)<<16);
			*(uint32_t*)(dst+SCREEN_WIDTH  ) = e|(DARKER(e,f)<<16);
			*(uint32_t*)(dst+SCREEN_WIDTH*2) = c|(DARKER(c,d)<<16);

			c = *(src+pitch16+2);
			a = *(src+2);
			e = DARKER(a,c);

			*(uint32_t*)(dst+                 2) = b|(a<<16);
			*(uint32_t*)(dst+SCREEN_WIDTH+    2) = f|(e<<16);
			*(uint32_t*)(dst+(SCREEN_WIDTH*2)+2) = d|(c<<16);

			b = *(src+3);
			d = *(src+pitch16+3);
			f = DARKER(b,d);

			*(uint32_t*)(dst+                 4) = DARKER(a,b)|(b<<16);
			*(uint32_t*)(dst+SCREEN_WIDTH+    4) = DARKER(e,f)|(f<<16);
			*(uint32_t*)(dst+(SCREEN_WIDTH*2)+4) = DARKER(c,d)|(d<<16);
		}
	}
}

/* drowsnug's nofilter upscaler, edited by eggs for smoothness */
static void scale_sharp_240x160_320xXXX(unsigned _w, unsigned _h, size_t _pitch, const void *src_bytes, void *dst_bytes)
{
	unsigned Eh = 0;
	int dh = 0;
	int width = 240;
	int vf = 0;
	const uint16_t *src = (const uint16_t *)src_bytes;
	uint16_t *dst = (uint16_t *)dst_bytes;

	dst += dst_offs / sizeof(uint16_t);

	unsigned x, y;
	for (y = 0; y < dst_h; y++)
	{
		int source = dh * width;
		for (x = 0; x < 320/4; x++)
		{
			register uint16_t a, b, c;

			a = src[source];
			b = src[source+1];
			c = src[source+2];

			if(vf == 1){
				a = AVERAGE16(a, src[source+width]);
				b = AVERAGE16(b, src[source+width+1]);
				c = AVERAGE16(c, src[source+width+2]);
			}

			*dst++ = a;
			*dst++ = AVERAGE16_1_3(a,b);
			*dst++ = AVERAGE16_1_3(c,b);
			*dst++ = c;
			source+=3;

		}
		Eh += 160;
		if(Eh >= dst_h) {
			Eh -= dst_h;
			dh++;
			vf = 0;
		}
		else
			vf = 1;
	}
}

static void scale_sharp_256xXXX_320xXXX(unsigned w, unsigned h, size_t pitch, const void *src_bytes, void *dst_bytes)
{
	unsigned Eh = 0;
	int dh = 0;
	int vf = 0;
	const uint16_t *src = (const uint16_t *)src_bytes;
	uint16_t *dst = (uint16_t *)dst_bytes;
	size_t pxpitch = pitch / 2;

	dst += dst_offs / sizeof(uint16_t);

	unsigned x, y;
	for (y = 0; y < dst_h; y++)
	{
		int source = dh * pxpitch;
		for (x = 0; x < 320/5; x++)
		{
			register uint16_t a, b, c, d;

			a = src[source];
			b = src[source+1];
			c = src[source+2];
			d = src[source+3];

			if(vf == 1){
				a = AVERAGE16(a, src[source+pxpitch]);
				b = AVERAGE16(b, src[source+pxpitch+1]);
				c = AVERAGE16(c, src[source+pxpitch+2]);
				d = AVERAGE16(d, src[source+pxpitch+3]);
			}

			*dst++ = a;
			*dst++ = AVERAGE16_1_3(a,b);
			*dst++ = AVERAGE16(b,c);
			*dst++ = AVERAGE16_1_3(d,c);
			*dst++ = d;
			source+=4;

		}
		Eh += h;
		if(Eh >= dst_h) {
			Eh -= dst_h;
			dh++;
			vf = 0;
		}
		else
			vf = 1;
	}
}

/* rotate 90° CW */
static void rotate_90cw(
	unsigned w, unsigned h, const uint16_t *src, uint16_t *dst)
{
	const size_t dst_pitch = h;
	for (unsigned y = 0; y < h; y++) {
		for (unsigned x = 0; x < w; x++) {
			dst[x * dst_pitch + (h - 1 - y)] = src[y * w + x];
		}
	}
}

/* rotate 180° */
static void rotate_180(
	unsigned w, unsigned h, const uint16_t *src, uint16_t *dst)
{
	const size_t total = w * h;
	for (size_t i = 0; i < total; i++) {
		dst[total - 1 - i] = src[i];
	}
}

/* rotate 270° CW */
static void rotate_270cw(
	unsigned w, unsigned h, const uint16_t *src, uint16_t *dst)
{
	const size_t dst_pitch = h;
	for (unsigned y = 0; y < h; y++) {
		for (unsigned x = 0; x < w; x++) {
			dst[(w - 1 - x) * dst_pitch + y] = src[y * w + x];
		}
	}
}

static void scale_select_scaler(unsigned w, unsigned h, size_t pitch) {
	double current_aspect_ratio;

	if (w == 0 || h == 0 || pitch == 0) {
		scaler = scale_null;
		return;
	};

	double real_ratio = (double)w / (double)h;

	/* Scaled mode: set correct aspect ratio for resolutions up to 320x240 */
	if (real_ratio <= 10.0f / 7.0f) {
		current_aspect_ratio = real_ratio;
	} else {
		/* 4:3 aspect ratio is forced for exotic resolutions (384x224 for CPS systems, 640x240 for PS1...) */
		current_aspect_ratio = aspect_ratio > 0 ? aspect_ratio : real_ratio;
	}

	/* MAME 2000 sets resolutions / aspect ratio without notifying
	 * of changes, new should always override old */
	if (!strcmp(core_name, "mame2000")) {
		current_aspect_ratio = ((double)w / (double)h);
	}

	scaler = NULL;

	if (blend_args.blend_line != NULL) {
		free(blend_args.blend_line);
		blend_args.blend_line = NULL;
	}

	if (scale_size == SCALE_SIZE_NATIVE) {
		/* Perfect 1:1 passthrough: behave exactly like SCALED for trivial cases */
		if (!strcmp(core_name, "mame2000") && video_width == 240 && video_height == 320) {
			scale_size = SCALE_SIZE_SCALED; /* force reuse of the scaled path */
			scale_select_scaler(w, h, pitch);
			return;
		}

		int dst_x, dst_y;
		src_offs = 0;

		if (w <= 320) {
			dst_x = ((SCREEN_WIDTH - (short)w) / 2);
		} else {
			/* Crop to 320px maximum. If larger, scale down after crop. */
			int src_w;
			if (strstr(core_name, "snes9x")) {
				/* For SNES, keep aspect ratio same for hi-res and normal */
				src_w = SCREEN_WIDTH * 2;
			} else {
				src_w = w * ((double)SCREEN_WIDTH / (double)320);
			}

			dst_x = ((src_w - (short)w) / 2);
		}

		dst_y = ((SCREEN_HEIGHT - (short)h) / 2);
		dst_w = w;
		dst_h = h;

		if (dst_y < 0) {
			dst_y = 0;
			dst_h = SCREEN_HEIGHT;
		}

		if (dst_x < 0) {
			src_offs += -dst_x * SCREEN_BPP;
			w_offs = dst_x * 2;
			w += w_offs;
			dst_x = 0;
			dst_w = SCREEN_WIDTH;
		}

		dst_offs = dst_y * SCREEN_PITCH + dst_x * SCREEN_BPP;
	} else if (scale_size == SCALE_SIZE_STRETCHED) {
		dst_w = SCREEN_WIDTH;
		dst_h = SCREEN_HEIGHT;
		dst_offs = 0;
	} else if (scale_size == SCALE_SIZE_SCALED) {
		/* Perfect 1:1 passthrough: behave exactly like NATIVE for GBA */
		if (w == 240 && h == 160) {
			scale_size = SCALE_SIZE_NATIVE; /* force reuse of the native path */
			scale_select_scaler(w, h, pitch);
			return;
		}

		dst_w = SCREEN_WIDTH;
		dst_h = SCREEN_WIDTH / current_aspect_ratio + 0.5;
		dst_offs = ((SCREEN_HEIGHT-dst_h)/2) * SCREEN_PITCH;

		if (dst_h > SCREEN_HEIGHT) {
			dst_w = SCREEN_HEIGHT * current_aspect_ratio + 0.5;
			dst_h = SCREEN_HEIGHT;
			dst_offs = ((SCREEN_WIDTH-dst_w)/2) * SCREEN_BPP;
		}
	} else if (scale_size == SCALE_SIZE_CROPPED) {
		/* Perfect 1:1 passthrough: behave exactly like NATIVE for trivial cases */
		if (w == 320 || w == 384 || h == 240) {
			scale_size = SCALE_SIZE_NATIVE; /* force reuse of the native path */
			scale_select_scaler(w, h, pitch);
			return;
		/* Perfect 1:1 passthrough: behave exactly like SCALED for trivial cases */
		} else if (!strcmp(core_name, "mame2000") && video_width == 240 && video_height == 320) {
			scale_size = SCALE_SIZE_SCALED; /* force reuse of the scaled path */
			scale_select_scaler(w, h, pitch);
			return;
		}

		/* Mode CROPPED (maximize height, keep aspect ratio, center both axes) */
		double src_aspect = (double)w / (double)h;

		/* Use full screen height */
		dst_h = SCREEN_HEIGHT;
		dst_w = (unsigned)(dst_h * src_aspect + 0.5);

		/* Limit width to avoid excessive overscan */
		if (dst_w > 320) {
			dst_w = 320;
			/* Do not apply aspect ratio for PS1 (must be always 4:3) and CPS1/2/3 (force 10:7) */
			if (!strstr(core_name, "pcsx") && (w != 384 && h != 224)) {
				dst_h = (unsigned)(dst_w / src_aspect + 0.5);
			}
		}

		int crop_x = 0;
		int visible_w = SCREEN_WIDTH; /* e.g. 240 px display width */
		src_offs = 0;
		w_offs = 0;

		/* Determine whether we will perform horizontal cropping.
		 * Normally cropping happens when the computed dst_w is wider than the visible screen.
		 * However some cores (MAME 2000) may provide portrait sources (240x320) and we still
		 * want to apply certain visual center fixes for these; handle those safely below.
		 */
		bool will_crop = (dst_w > visible_w);

		/* If portrait vertical game for MAME 2000 (240x320) it may need the visual fix.
		 * We treat it specially: we do NOT perform a real "crop" (dst_w <= visible_w),
		 * but we still allow a small horizontal centering adjustment (visual_center_fix).
		 * This is safe because we clamp the computed offsets afterwards.
		 */
		bool special_portrait_mame = (!strcmp(core_name, "mame2000") && w == 240 && h == 320);

		if (will_crop || special_portrait_mame) {
			/* compute nominal crop center */
			crop_x = (dst_w > visible_w) ? (dst_w - visible_w) / 2 : 0;

			int visual_center_fix = 0;

			/* Base per-system offsets (empirical) */
			if      (w == 96 && h == 64)   { visual_center_fix = 30; } /* Pokémon Mini */
			else if (w == 160 && h == 102) { visual_center_fix = 20; } /* Lynx */
			else if (w == 160 && h == 144) { visual_center_fix = 5;  } /* GB, GBC, GG */
			else if (w == 160 && h == 152) { visual_center_fix = 2;  } /* NGP */
			else if (w == 224)             { visual_center_fix = 12; } /* WonderSwan */
			else if (w == 240)             { visual_center_fix = 10; } /* GBA */
			else if (w == 304)             { visual_center_fix = 2;  } /* Neo-Geo */
			else if (w == 384)             { visual_center_fix = -8; } /* CPS1/2/3 */
			else if (w == 512)             { visual_center_fix = -24;} /* PS1 */
			else if (w == 640)             { visual_center_fix = -40;} /* PS1 alt */

			/* Apply the visual center fix */
			crop_x -= visual_center_fix;

			/* Clamp crop_x to safe range. Always ensure we do not generate negative or out-of-range offsets. */
			if (crop_x < 0) crop_x = 0;
			if (dst_w >= visible_w) {
				if (crop_x + visible_w > (int)dst_w)
					crop_x = dst_w - visible_w;
			} else {
				/* if dst_w < visible_w (no real crop) ensure crop_x is within source bounds */
				if (crop_x + visible_w > (int)dst_w)
					crop_x = 0; /* fallback to center / no shift */
			}

			/* compute source offset in bytes, and optional w_offs (used by some native/crop logic) */
			src_offs = crop_x * SCREEN_BPP;

			/* w_offs previously used when dst_x < 0 in native; keep 0 here unless special handling required */
			w_offs = 0;

			PA_INFO("[CROPPED] compute crop: src=%ux%u dst=%ux%u crop_x=%d visual_fix=%d\n",
					w, h, dst_w, dst_h, crop_x, visual_center_fix);
		}

		/* Center vertically only */
		int dst_x = (SCREEN_WIDTH - visible_w) / 2;
		int dst_y = (SCREEN_HEIGHT - (short)dst_h) / 2;
		if (dst_x < 0) dst_x = 0;
		if (dst_y < 0) dst_y = 0;

		/* If we actually have a crop (dst_w > visible_w) then we render at left of visible area
		 * and the crop_scaler will read from src + src_offs; dst_x should remain 0 (we want the
		 * visible rectangle to be located starting at 0). For other cases, we center horizontally.
		 */
		if (dst_w > visible_w) {
			dst_x = 0;
		} else {
			/* center the smaller rendered area in the visible width */
			dst_x = (visible_w - (int)dst_w) / 2;
			if (dst_x < 0) dst_x = 0;
		}

		dst_offs = dst_y * SCREEN_PITCH + dst_x * SCREEN_BPP;

		/* Optimization: skip scaling if src == dst (no transform needed) */
		bool native_match = false;
		if (abs((int)dst_w - (int)w) <= 1 && abs((int)dst_h - (int)h) <= 1) {
			dst_w = w;
			dst_h = h;
			native_match = true;
		}

		/* After potential native_match change, recompute dst_x/dst_offs/src_offs safely again
		 * (this prevents stale offsets that could point outside the framebuffer).
		 */
		if (native_match) {
			if (dst_w > visible_w) {
				/* cropping still applies */
				dst_x = 0;
			} else {
				dst_x = (visible_w - (int)dst_w) / 2;
				if (dst_x < 0) dst_x = 0;
			}
			dst_offs = dst_y * SCREEN_PITCH + dst_x * SCREEN_BPP;

			/* If we changed the logical w used by scalers, be conservative and reset w_offs */
			w_offs = 0;
			/* src_offs already clamped above */
		}

		/* Select scaler - make blend buffer allocation use safe size (max of src/dst width) */
		if (native_match) {
			scaler = scale_1x; /* direct copy, no resampling */
		} else if (scale_filter == SCALE_FILTER_NEAREST) {
			scaler = scale_nearest;
		} else if (scale_filter == SCALE_FILTER_SHARP || scale_filter == SCALE_FILTER_SMOOTH) {
			int gcd_w, div_w, gcd_h, div_h;
			size_t blend_w = (size_t) ( (w > (int)dst_w) ? w : dst_w );
			blend_args.blend_line = calloc(blend_w, sizeof(uint16_t));
			if (!blend_args.blend_line) {
				PA_ERROR("calloc failed for blend_line (%zu)\n", blend_w);
				/* fallback to simple scaler to avoid crash */
				scaler = scale_nearest;
			} else {
				gcd_w = gcd(w, dst_w);
				blend_args.w_ratio_in = w / gcd_w;
				blend_args.w_ratio_out = dst_w / gcd_w;

				div_w = (blend_args.w_ratio_out + 2) / 5;
				blend_args.w_bp[0] = div_w;
				blend_args.w_bp[1] = blend_args.w_ratio_out >> 1;

				gcd_h = gcd(h, dst_h);
				blend_args.h_ratio_in = h / gcd_h;
				blend_args.h_ratio_out = dst_h / gcd_h;

				div_h = (blend_args.h_ratio_out + 2) / 5;
				blend_args.h_bp[0] = div_h;
				blend_args.h_bp[1] = blend_args.h_ratio_out >> 1;

				scaler = scale_blend;
			}
		} else {
			scaler = scale_1x;
		}

		/* Switch to cropped scaler if needed */
		if (dst_w > visible_w) {
			crop_scaler = scaler;
			scaler = scale_crop;
		}

		/* Final safety checks before returning: ensure dst_offs within framebuffer and widths are sane */
		if ((size_t)(dst_x + dst_w) > (size_t)SCREEN_WIDTH) {
			PA_ERROR("Safety clamp: dst_x+dst_w (%d + %u) > SCREEN_WIDTH (%d). Clamping dst_x.\n",
					 dst_x, dst_w, SCREEN_WIDTH);
			if ((int)dst_w <= SCREEN_WIDTH)
				dst_x = (SCREEN_WIDTH - (int)dst_w) / 2;
			else
				dst_x = 0;
			dst_offs = dst_y * SCREEN_PITCH + dst_x * SCREEN_BPP;
		}

		PA_INFO("[CROPPED] final src=%ux%u dst=%ux%u crop_x=%d offs=%d,%d\n",
				w, h, dst_w, dst_h, crop_x, dst_x, dst_y);
	}

	if (!scaler && w == 160 && h == 144) {
		if (scale_size == SCALE_SIZE_SCALED && scale_filter == SCALE_FILTER_SHARP) {
			unsigned dst_x = ((SCREEN_WIDTH - 240) * SCREEN_BPP / 2);
			unsigned dst_y = ((SCREEN_HEIGHT - 216) / 2);
			dst_offs = dst_y * SCREEN_PITCH + dst_x;

			scaler = scale_sharp_160x144_240x216;
			return;
		}
	}

	if (SCREEN_WIDTH == 320 && scale_filter == SCALE_FILTER_SHARP) {
		if (!scaler && w == 240 && h == 160) {
			scaler = scale_sharp_240x160_320xXXX;
			return;
		}

		if (!scaler &&
		    w == 256 &&
		    (current_aspect_ratio == 4.0f / 3.0f || scale_size == SCALE_SIZE_STRETCHED))
		{
			scaler = scale_sharp_256xXXX_320xXXX;
			return;
		}
	}

	if (!scaler && scale_filter == SCALE_FILTER_NEAREST) {
		scaler = scale_nearest;
	}

	if (!scaler && (scale_filter == SCALE_FILTER_SHARP || scale_filter == SCALE_FILTER_SMOOTH)) {
		int gcd_w, div_w, gcd_h, div_h;
		blend_args.blend_line = calloc(w, sizeof(uint16_t));

		gcd_w = gcd(w, dst_w);
		blend_args.w_ratio_in = w / gcd_w;
		blend_args.w_ratio_out = dst_w / gcd_w;

		div_w = (blend_args.w_ratio_out + 2) / 5; /* rounded integer divide by 5 */
		blend_args.w_bp[0] = div_w;
		blend_args.w_bp[1] = blend_args.w_ratio_out >> 1;

		gcd_h = gcd(h, dst_h);
		blend_args.h_ratio_in = h / gcd_h;
		blend_args.h_ratio_out = dst_h / gcd_h;

		div_h = (blend_args.h_ratio_out + 2) / 5; /* rounded integer divide by 5 */
		blend_args.h_bp[0] = div_h;
		blend_args.h_bp[1] = blend_args.h_ratio_out >> 1;

		scaler = scale_blend;
	}

	if (!scaler) {
		scaler = scale_1x;
	}

	if (scale_size == SCALE_SIZE_NATIVE) {
		if (w <= SCREEN_WIDTH && h <= SCREEN_HEIGHT) {
			crop_scaler = scale_1x;
		} else {
			crop_scaler = scaler;
		}
		scaler = scale_crop;
	}
}

void scale_update_scaler(void) {
	scale_select_scaler(prev.w, prev.h, prev.pitch);
}

void scale(unsigned w, unsigned h, size_t pitch, const void *src, void *dst)
{
	video_width  = w;
	video_height = h;

	static uint16_t *tmpbuf = NULL;
	static size_t tmp_size = 0;

	// Re-select the scaling function if the source dimensions or pitch have changed
	if (w != prev.w || h != prev.h || pitch != prev.pitch) {
		PA_INFO("Dimensions changed to %dx%d\n", w, h);
		scale_select_scaler(w, h, pitch);
		prev.w = w; prev.h = h; prev.pitch = pitch;
	}

	// ---- Normal (non-rotated) rendering ----
	if (!rotate_display) {
		// Directly render the scaled frame into the destination buffer
		// (no need to clear since scaler() overwrites the entire area)
		scaler(w, h, pitch, src, dst);
		return;
	}

	// ----- Rotated rendering -----
	if (rotate_display != 0) {
		/* allocate tmpbuf with the actual framebuffer stride (bytes per line) to match scalers */
		size_t needed = (size_t)SCREEN_PITCH * (size_t)SCREEN_HEIGHT; /* bytes */
		if (tmp_size < needed) {
			free(tmpbuf);
			tmpbuf = malloc(needed);
			if (!tmpbuf) {
				PA_ERROR("malloc failed for tmpbuf (%zu bytes)\n", needed);
				/* fallback: render directly to dst (may crash otherwise) */
				scaler(w, h, pitch, src, dst);
				return;
			}
			tmp_size = needed;
		}

		/* Clear tmpbuf so regions not written by scaler remain black (prevents residues) */
		memset(tmpbuf, 0, needed);

		/* Step 1 : render normally into tmpbuf (scalers expect dst stride == SCREEN_PITCH) */
		scaler(w, h, pitch, src, tmpbuf);

		/* Step 2 : apply selected rotation. tmpbuf uses SCREEN_PITCH stride so rotation functions must read with that stride. */
		switch (rotate_display) {

			case 1: // 90° CW
				rotate_90cw(SCREEN_WIDTH, SCREEN_HEIGHT, (const uint16_t *)tmpbuf, (uint16_t *)dst);
				break;

			case 2: // 180°
				rotate_180(SCREEN_WIDTH, SCREEN_HEIGHT, (const uint16_t *)tmpbuf, (uint16_t *)dst);
				break;

			case 3: // 270° CW
				rotate_270cw(SCREEN_WIDTH, SCREEN_HEIGHT, (const uint16_t *)tmpbuf, (uint16_t *)dst);
				break;
		}
		return;
	}
}
