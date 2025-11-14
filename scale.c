#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "main.h"
#include "options.h"
#include "scale.h"

int video_width  = 0;
int video_height = 0;

/* If set, the next frame will clear the full framebuffer to avoid residues
 * after mode changes (zoom, rotation, scaler change). This is set by
 * scale_compute_zoomed() and cleared in scale() when used. */
int need_full_clear = 0;

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

/* --- Zoom helper forward declaration --- */
static void scale_compute_zoomed(unsigned w, unsigned h, size_t pitch);

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

	// --- Fix: ensure last destination line (truncated) is not visible ---
	{
		uint8_t *base = (uint8_t *)dst - SCREEN_PITCH; // go back one line
		uint8_t *last = base + SCREEN_PITCH;           // point to final line
		memset(last, 0, SCREEN_PITCH);
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

	// --- Fix: ensure last destination line (truncated) is not visible ---
	{
		uint8_t *base = (uint8_t *)dst - SCREEN_PITCH; // go back one line
		uint8_t *last = base + SCREEN_PITCH;           // point to final line
		memset(last, 0, SCREEN_PITCH);
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
		} else {
			vf = 1;
		}
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
		} else {
			vf = 1;
		}
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
	if (strstr(core_name, "mame2000")) {
		current_aspect_ratio = ((double)w / (double)h);
	}

	/* ---------- ZOOMED mode is exclusive: compute and return early ----------
	 * Handle zoomed mode as the first case so that the rest of the scaler
	 * selection logic (SCALED/CROPPED/NATIVE/etc.) does not override the
	 * dst_* / src_offs / scaler values computed by scale_compute_zoomed().
	 */
	if (scale_size == SCALE_SIZE_ZOOMED) {
		scale_compute_zoomed(w, h, pitch);
		return;
	}

	scaler = NULL;

	if (blend_args.blend_line != NULL) {
		free(blend_args.blend_line);
		blend_args.blend_line = NULL;
	}

	if (scale_size == SCALE_SIZE_NATIVE) {
		/* Perfect 1:1 passthrough: behave exactly like SCALED for trivial cases */
		if (strstr(core_name, "mame2000") && video_width == 240 && video_height == 320) {
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
		if (h == 240 || strstr(core_name, "pcsx")) {
			scale_size = SCALE_SIZE_NATIVE; /* force reuse of the native path */
			scale_select_scaler(w, h, pitch);
			return;
		/* Perfect 1:1 passthrough: behave exactly like SCALED for trivial cases */
		} else if (strstr(core_name, "mame2000") && video_width == 240 && video_height == 320) {
			scale_size = SCALE_SIZE_SCALED; /* force reuse of the scaled path */
			scale_select_scaler(w, h, pitch);
			return;
		}

		/* Mode CROPPED (maximize height, keep aspect ratio, center both axes) */
		double src_aspect = (double)w / (double)h;

		/* Use full screen height */
		dst_h = SCREEN_HEIGHT;
		dst_w = (unsigned)(dst_h * src_aspect + 0.5);

		/* Do not apply aspect ratio for CPS1/2/3 (force 10:7) */
		if (w == 384 && h == 224) {
			dst_w = SCREEN_HEIGHT * (10.0f / 7.0f);
			dst_h = SCREEN_HEIGHT;
		} else {
			dst_h = (unsigned)(dst_w / src_aspect + 0.5);
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
		bool special_portrait_mame = (strstr(core_name, "mame2000") && w == 240 && h == 320);

		if (will_crop || special_portrait_mame) {
			/* compute nominal crop center */
			crop_x = (dst_w > visible_w) ? (dst_w - visible_w) / 2 : 0;

			int visual_center_fix = 0;

			/* Base per-system offsets (empirical) */
			if      (w == 96 && h == 64)   { visual_center_fix = 30; } /* Pokémon Mini */
			else if (w == 160 && h == 102) { visual_center_fix = 39; } /* Lynx */
			else if (w == 160 && h == 144) { visual_center_fix = 5;  } /* GB, GBC, GG */
			else if (w == 160 && h == 152) { visual_center_fix = 2;  } /* NGP */
			else if (w == 224 && h == 144) { visual_center_fix = 26; } /* WonderSwan */
			else if (w == 240 && h == 160) { visual_center_fix = 20; } /* GBA */
			else if (w == 256 && h == 192) { visual_center_fix = 8;  } /* Master System */
			else if (w == 256 && h == 224) { visual_center_fix = 1;  } /* SNES */
			else if (w == 304 && h == 224) { visual_center_fix = 3;  } /* Neo-Geo */
			else if (w == 320 && h == 224) { visual_center_fix = 3;  } /* Genesis */
			else if (w == 384 && h == 224) { visual_center_fix = -6; } /* CPS1/2/3 */
			else if (w == 384 && h == 256) { visual_center_fix = -4; } /* Irem */
			else if (w == 395 && h == 254) { visual_center_fix = -4; } /* Midway */
			else if (w == 512)             { visual_center_fix = -24;} /* PS1 */
			else if (w == 640)             { visual_center_fix = -40;} /* PS1 alt */

			/* Apply the visual center fix */
			crop_x -= visual_center_fix;

			/* Clamp crop_x to safe range. Always ensure we do not generate negative or out-of-range offsets. */
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
		memset(dst, 0, SCREEN_HEIGHT * SCREEN_PITCH);
		prev.w = w; prev.h = h; prev.pitch = pitch;
	}

	// ---- Normal (non-rotated) rendering ----
	if (!rotate_display) {
		// If requested, clear whole framebuffer once to avoid residues from previous modes/zooms.
		if (need_full_clear) {
			memset(dst, 0, (size_t)SCREEN_PITCH * (size_t)SCREEN_HEIGHT);
			need_full_clear = 0;
		}

		// Directly render the scaled frame into the destination buffer
		// (scaler should overwrite the rendered region)
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

static void scale_compute_zoomed(unsigned w, unsigned h, size_t pitch)
{
	/* Rewrittewn zoomed scaler selection + centering logic.
	 * This version:
	 *  - enforces special aspect overrides (CPS / PS1) earlier
	 *  - computes a virtual output canvas size (dst_w/dst_h)
	 *  - interpolates centers (subpixel-safe)
	 *  - when virtual width > SCREEN_WIDTH (crop path) we keep dst_x==0
	 *    and compute the proper src_offs by mapping the desired virtual
	 *    viewport (Vx) into source pixels (safe scaling + rounding).
	 *
	 * Debugging: set zoom_debug = 1 to print helpful values via PA_INFO.
	 */
	double aspect = (double)w / (double)h;
	double zoom = zoom_level / 100.0;
	static int zoom_debug = 0; /* set to 1 for runtime debugging prints */
	/* ---------------------------------------------------------
	 * Forced aspect-ratio overrides for known odd systems
	 * --------------------------------------------------------- */
	if (w == 384 && h == 224) {
		aspect = 10.0f / 7.0f;  /* CPS1/2/3 historical target */
	} else if (strstr(core_name, "pcsx")) {
		aspect = 4.0f / 3.0f;   /* PS1 correction */
	}
	/* ---------------------------------------------------------
	 * 1) TRUE BASE (zoom 0%) - choose native vs scaled base
	 * --------------------------------------------------------- */
	unsigned base_w = w;
	unsigned base_h = h;
	if (w <= 240) {
		/* native 1:1 base */
		base_w = w;
		base_h = h;
	} else {
		/* scaled base (fit to SCREEN_WIDTH keeping aspect) */
		base_w = SCREEN_WIDTH;
		base_h = (int)(SCREEN_WIDTH / aspect + 0.5);
		if (base_h > SCREEN_HEIGHT) {
			base_h = SCREEN_HEIGHT;
			base_w = (int)(SCREEN_HEIGHT * aspect + 0.5);
		}
	}
	/* ---------------------------------------------------------
	 * 2) FULL CROPPED (zoom 100%) - virtual full-canvas size
	 * --------------------------------------------------------- */
	unsigned full_crop_h = SCREEN_HEIGHT;
	unsigned full_crop_w = (unsigned)(full_crop_h * aspect + 0.5);
	/* ---------------------------------------------------------
	 * 3) VIRTUAL OUTPUT (interpolate between base and full)
	 * --------------------------------------------------------- */
	dst_w = base_w + (unsigned)((double)(full_crop_w - base_w) * zoom);
	dst_h = base_h + (unsigned)((double)(full_crop_h - base_h) * zoom);
	if (dst_w < 1) dst_w = 1;
	if (dst_h < 1) dst_h = 1;
	/* Compute base position (centered by default) */
	int base_x = (SCREEN_WIDTH - (int)base_w) / 2;
	int base_y = (SCREEN_HEIGHT - (int)base_h) / 2;
	/* final target when zoom == 1:
	 * - if virtual canvas wider than screen -> we will use crop (dst_x==0)
	 * - otherwise center the virtual canvas inside the screen
	 */
	int final_dst_x = (dst_w > SCREEN_WIDTH) ? 0 : (SCREEN_WIDTH - (int)dst_w) / 2;
	int final_dst_y = (SCREEN_HEIGHT - (int)dst_h) / 2;
	if (final_dst_y < 0) final_dst_y = 0;
	/* Interpolate *centers* for symmetry and to avoid integer bias */
	double base_cx = base_x + ((double)base_w / 2.0);
	double base_cy = base_y + ((double)base_h / 2.0);
	double final_cx = final_dst_x + ((double)dst_w / 2.0);
	double final_cy = final_dst_y + ((double)dst_h / 2.0);
	double cx = base_cx + (final_cx - base_cx) * zoom;
	double cy = base_cy + (final_cy - base_cy) * zoom;
	/* ideal dst (if we could move framebuffer) */
	int dst_x_ideal = (int)(cx - ((double)dst_w / 2.0) + 0.5);
	int dst_y_ideal = (int)(cy - ((double)dst_h / 2.0) + 0.5);
	/* By default we will write into framebuffer at centered position.
	 * But if dst_w > SCREEN_WIDTH we must use crop path (dst_x==0)
	 * and shift source (src_offs) so the visible window is centered.
	 */
	if (zoom_debug) {
		PA_INFO("[ZOOMDBG] w=%u h=%u aspect=%.6f zoom=%.3f base=%ux%u full=%ux%u dst=%ux%u\n",
				w, h, aspect, zoom, base_w, base_h, full_crop_w, full_crop_h, dst_w, dst_h);
		PA_INFO("[ZOOMDBG] base_cx=%.2f final_cx=%.2f cx=%.2f dst_x_ideal=%d dst_y_ideal=%d\n",
				base_cx, final_cx, cx, dst_x_ideal, dst_y_ideal);
	}
	/* Compute visible source width (how many source pixels correspond to visible SCREEN_WIDTH
	 * when the virtual canvas is full_crop_w). This is used to bound the source offset.
	 */
	unsigned visible_w = SCREEN_WIDTH;
	double visible_src_w = (full_crop_w > 0)
		? (double)w * ((double)visible_w / (double)full_crop_w)
		: (double)w;
	/* Map the desired virtual viewport into source coordinates when cropping. */
	if (dst_w > SCREEN_WIDTH) {
		/* Virtual canvas is larger than the screen -> we must crop.
		 *
		 * We want the visible window of width SCREEN_WIDTH to be strictly centered
		 * on the virtual canvas, regardless of dst_x_ideal.
		 * So, the visible window starts at:
		 *
		 *   Vx = (dst_w - SCREEN_WIDTH) / 2
		 *
		 * This ensures the visible part is always centered.
		 */
		double Vx = ((double)dst_w - (double)SCREEN_WIDTH) / 2.0;
		double src_pixels_per_virtual = (double)w / (double)dst_w;
		double src_off_pixels = Vx * src_pixels_per_virtual;
		/* Compute byte offset and clamp */
		int new_src_offs = (int)(src_off_pixels * (double)SCREEN_BPP + 0.5);
		/* clamp to valid source range (in bytes) */
		int max_src_off_bytes = (int)((double)w - visible_src_w) * SCREEN_BPP;
		if (new_src_offs < 0) new_src_offs = 0;
		if (new_src_offs > max_src_off_bytes) new_src_offs = max_src_off_bytes;
		/* Align to pixel boundary */
		if (new_src_offs % SCREEN_BPP) {
			new_src_offs -= (new_src_offs % SCREEN_BPP);
			if (new_src_offs < 0) new_src_offs = 0;
		}
		src_offs = new_src_offs;
		/* destination must be left-aligned for crop path */
		dst_offs = 0; /* dst_x will be added below */
		dst_x_ideal = 0; /* ensure we don't try to write at dst_x */
		if (zoom_debug) {
			PA_INFO("[ZOOMDBG] CROP path: Vx=%.2f src_pixels_per_virtual=%.6f src_off_pixels=%.2f src_offs=%d max_src=%d\n",
					Vx, src_pixels_per_virtual, src_off_pixels, src_offs, max_src_off_bytes);
		}
	}

	/* Compute final dst_x/dst_y to write into framebuffer.
	 * If we are in crop path dst_x_ideal is now clamped but not forced to 0.
	 */
	int dst_x = dst_x_ideal;
	int dst_y = dst_y_ideal;
	/* Safety clamp final dst coords to visible framebuffer */
	if (dst_x < 0) dst_x = 0;
	if (dst_y < 0) dst_y = 0;
	if ((size_t)(dst_x + dst_w) > (size_t)SCREEN_WIDTH) {
		/* clamp to avoid writing beyond framebuffer when not in crop path */
		if ((int)dst_w <= SCREEN_WIDTH)
			dst_x = (SCREEN_WIDTH - (int)dst_w) / 2;
		else
			dst_x = 0;
	}
	/* Final dst_offs in bytes */
	dst_offs = dst_y * SCREEN_PITCH + dst_x * SCREEN_BPP;
	if (zoom_debug) {
		PA_INFO("[ZOOMDBG] final: dst_x=%d dst_y=%d dst_offs=%d dst_w=%u dst_h=%u src_offs=%d\n",
				dst_x, dst_y, dst_offs, dst_w, dst_h, src_offs);
	}
	/* Prevent fractional alignment for blend modes (safety) */
	if (src_offs % SCREEN_BPP) {
		src_offs -= (src_offs % SCREEN_BPP);
		if (src_offs < 0) src_offs = 0;
	}
	w_offs = 0;
	/* ---------------------------------------------------------
	 * SCALER SELECTION (same logic as before, but using new dst_w/dst_h)
	 * --------------------------------------------------------- */
	if (scale_filter == SCALE_FILTER_NEAREST) {
		scaler = scale_nearest;
	}
	else if (scale_filter == SCALE_FILTER_SHARP || scale_filter == SCALE_FILTER_SMOOTH) {
		int gcd_w = gcd((int)w, (int)dst_w);
		int div_w = dst_w / gcd_w;
		blend_args.w_ratio_in  = w / gcd_w;
		blend_args.w_ratio_out = dst_w / gcd_w;
		blend_args.w_bp[0]     = (div_w + 2) / 5;
		blend_args.w_bp[1]     = div_w >> 1;
		int gcd_h = gcd((int)h, (int)dst_h);
		int div_h = dst_h / gcd_h;
		blend_args.h_ratio_in  = h / gcd_h;
		blend_args.h_ratio_out = dst_h / gcd_h;
		blend_args.h_bp[0]     = (div_h + 2) / 5;
		blend_args.h_bp[1]     = div_h >> 1;
		if (blend_args.blend_line)
			free(blend_args.blend_line);
		size_t blend_w = (size_t)((w > (int)dst_w) ? w : dst_w);
		blend_args.blend_line = calloc(blend_w, sizeof(uint16_t));
		if (!blend_args.blend_line)
			scaler = scale_nearest;
		else
			scaler = scale_blend;
	}
	else {
		scaler = scale_1x;
	}
	/* If virtual canvas is larger than visible window, switch to crop path */
	if (dst_w > SCREEN_WIDTH) {
		crop_scaler = scaler;
		scaler = scale_crop;
		PA_INFO("[ZOOMED] using crop path: virtual dst_w=%u visible=%u src_offs=%d\n",
				dst_w, SCREEN_WIDTH, src_offs);
	}
	need_full_clear = 1;
}
