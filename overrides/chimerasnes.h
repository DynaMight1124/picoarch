#include "overrides.h"

static const struct core_override_option chimerasnes_core_option_overrides[] = {
	{
		.key = "chimerasnes_frameskip",
		.info = "Improves performance at the expense of visual smoothness. 'Auto' skips frames and 'Manual' uses the 'FS Threshold' setting.",
	},
	{
		.key = "chimerasnes_frameskip_threshold",
		.desc = "FS Threshold",
		.info = "When FS is set to 'Manual', specifies the audio buffer threshold below which frames will be skipped. Higher values reduce the risk of crackling.",
	},
	{
		.key = "chimerasnes_bsx_bios",
		.options = {
			{ "skip", "Skip" },
			{ "load", "Load" },
		}
	},
	{
		.key = "chimerasnes_overclock_cycles",
		.desc = "Reduce Slowdown",
		.info = "Overclock SNES CPU. May cause games to crash! Use 'AutoFastROM' for shorter loadings, 'Low' for games slowdown and 'High' if required (Restart).",
	},
	{
		.key = "chimerasnes_reduce_sprite_flicker",
		.desc = "Reduce Flickering",
	},
	{
		.key = "chimerasnes_overclock_superfx",
		.info = "Overclock or underclock the SuperFX chip. This may improve the framerate and playability of games that use SuperFX (10 Mhz by default).",
		.options = {
			{ "5 MHz",           NULL },
			{ "6 MHz",           NULL },
			{ "7 MHz",           NULL },
			{ "8 MHz",           NULL },
			{ "9 MHz",           NULL },
			{ "10 MHz",          NULL },
			{ "11 MHz",          NULL },
			{ "12 MHz",          NULL },
			{ "13 MHz",          NULL },
			{ "14 MHz",          NULL },
			{ "15 MHz",          NULL },
			{ "20 MHz",          NULL },
			{ "30 MHz",          NULL },
			{ "40 MHz",          NULL },
		}
	},
	{ NULL }
};

me_bind_action chimerasnes_ctrl_actions[] =
{
	{ "UP       ",  1 << RETRO_DEVICE_ID_JOYPAD_UP},
	{ "DOWN     ",  1 << RETRO_DEVICE_ID_JOYPAD_DOWN },
	{ "LEFT     ",  1 << RETRO_DEVICE_ID_JOYPAD_LEFT },
	{ "RIGHT    ",  1 << RETRO_DEVICE_ID_JOYPAD_RIGHT },
	{ "A BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_A },
	{ "B BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_B },
	{ "X BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_X },
	{ "Y BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_Y },
	{ "START    ",  1 << RETRO_DEVICE_ID_JOYPAD_START },
	{ "SELECT   ",  1 << RETRO_DEVICE_ID_JOYPAD_SELECT },
	{ "L BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_L },
	{ "R BUTTON ",  1 << RETRO_DEVICE_ID_JOYPAD_R },
	{ NULL,       0 }
};

#define chimerasnes_overrides {                              \
	.core_name = "chimerasnes",                          \
	.actions = chimerasnes_ctrl_actions,                 \
	.action_size = array_size(chimerasnes_ctrl_actions), \
	.options = chimerasnes_core_option_overrides         \
}
