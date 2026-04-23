#include "overrides.h"

static const struct core_override_option ecwolf_core_option_overrides[] = {
	{
		.key = "ecwolf-resolution",
		.default_value = "320x240",
		.blocked = true,
	},
	{
		.key = "ecwolf-analog-deadzone",
		.blocked = true,
	},
	{
		.key = "ecwolf-fps",
		.default_value = "60",
	},
	{
		.key = "ecwolf-palette",
		.default_value = "rgb565",
		.blocked = true,
	},
	{
		.key = "ecwolf-viewsize",
		.options = {
			[16] = {"20", "With statusbar"},
			[17] = {"21", "No statusbar"},
		}
	},
	{
		.key = "ecwolf-am-overlay",
		.desc = "Show overlay map",
		.options = {
			[2] = {"both", "Both"},
		}
	},
	{
		.key = "ecwolf-am-drawtexturedwalls",
		.desc = "Textured walls",
	},
	{
		.key = "ecwolf-am-drawtexturedfloors",
		.desc = "Textured floors",
	},
	{
		.key = "ecwolf-am-texturedoverlay",
		.desc = "Textured Overlay",
	},
	{
		.key = "ecwolf-am-showratios",
		.desc = "Show level ratios",
	},
	{
		.key = "ecwolf-am-pause",
		.blocked = true,
	},
	{
		.key = "ecwolf-dynamic-fps",
		.blocked = true,
	},
	{
		.key = "ecwolf-invulnerability",
		.blocked = true,
	},
	{
		.key = "ecwolf-am-pause",
		.desc = "Pause game",
	},
	{
		.key = "ecwolf-digi-volume",
		.desc = "Volume of SFX",
	},
	{
		.key = "ecwolf-adlib-volume",
		.desc = "Volume of Adlib SFX",
	},
	{
		.key = "ecwolf-speaker-volume",
		.desc = "Vol. of Speaker SFX",
	},
	{
		.key = "ecwolf-analog-move-sensitivity",
		.blocked = true,
	},
	{
		.key = "ecwolf-analog-turn-sensitivity",
		.blocked = true,
	},
	{
		.key = "ecwolf-effects-priority",
		.desc = "SFX Lookup order",
		.options = {
			{ "digi-adlib-speaker", "Dig, Ad, Sp" },
			{ "digi-adlib", "Dig, Ad" },
			{ "digi-speaker", "Dig, Sp" },
			{ "digi", "Dig only" },
			{ "adlib", "Ad only" },
			{ "speaker", "Sp only" },
			{ NULL, NULL },
		}
	},
	{
		.key = "ecwolf-memstore",
		.desc = "Store files",
	},
	{
		.key = "ecwolf-preload-digisounds",
		.desc = "Preload sounds",
	},
	{
		.key = "ecwolf-panx-adjustment",
		.desc = "Horiz. panning",
	},
	{
		.key = "ecwolf-pany-adjustment",
		.desc = "Vert. panning",
	},
	{ NULL }
};

#ifdef FUNKEY_S
/* DrUm78 build */
me_bind_action ecwolf_ctrl_actions[] =
{
	{ "UP       ",  1 << RETRO_DEVICE_ID_JOYPAD_UP},
	{ "DOWN     ",  1 << RETRO_DEVICE_ID_JOYPAD_DOWN },
	{ "LEFT     ",  1 << RETRO_DEVICE_ID_JOYPAD_LEFT },
	{ "RIGHT    ",  1 << RETRO_DEVICE_ID_JOYPAD_RIGHT },
	{ "USE      ",  1 << RETRO_DEVICE_ID_JOYPAD_A },
	{ "RUN      ",  1 << RETRO_DEVICE_ID_JOYPAD_B },
	{ "FIRE     ",  1 << RETRO_DEVICE_ID_JOYPAD_X },
	{ "STRAFE   ",  1 << RETRO_DEVICE_ID_JOYPAD_Y },
	{ "MENU     ",  1 << RETRO_DEVICE_ID_JOYPAD_START },
	{ "MAP      ",  1 << RETRO_DEVICE_ID_JOYPAD_SELECT },
	{ "PREV WPN ",  1 << RETRO_DEVICE_ID_JOYPAD_L },
	{ "NEXT WPN ",  1 << RETRO_DEVICE_ID_JOYPAD_R },
	{ NULL,       0 }
};
#else
me_bind_action ecwolf_ctrl_actions[] =
{
	{ "UP       ",  1 << RETRO_DEVICE_ID_JOYPAD_UP},
	{ "DOWN     ",  1 << RETRO_DEVICE_ID_JOYPAD_DOWN },
	{ "LEFT     ",  1 << RETRO_DEVICE_ID_JOYPAD_LEFT },
	{ "RIGHT    ",  1 << RETRO_DEVICE_ID_JOYPAD_RIGHT },
	{ "USE      ",  1 << RETRO_DEVICE_ID_JOYPAD_A },
	{ "STRAFE   ",  1 << RETRO_DEVICE_ID_JOYPAD_B },
	{ "FIRE     ",  1 << RETRO_DEVICE_ID_JOYPAD_X },
	{ "RUN      ",  1 << RETRO_DEVICE_ID_JOYPAD_Y },
	{ "MENU     ",  1 << RETRO_DEVICE_ID_JOYPAD_START },
	{ "MAP      ",  1 << RETRO_DEVICE_ID_JOYPAD_SELECT },
	{ "STRAFE L ",  1 << RETRO_DEVICE_ID_JOYPAD_L },
	{ "STRAFE R ",  1 << RETRO_DEVICE_ID_JOYPAD_R },
	{ "PREV WPN ",  1 << RETRO_DEVICE_ID_JOYPAD_L2 },
	{ "NEXT WPN ",  1 << RETRO_DEVICE_ID_JOYPAD_R2 },
	{ NULL,       0 }
};

#endif

#define ecwolf_overrides {                           \
	.core_name = "ecwolf",                             \
	.actions = ecwolf_ctrl_actions,                    \
	.action_size = array_size(ecwolf_ctrl_actions),    \
	.options = ecwolf_core_option_overrides,           \
}
