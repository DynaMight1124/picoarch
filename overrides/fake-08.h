#include "overrides.h"

static const struct core_override_option fake08_core_option_overrides[] = {
	{
		.key = "fake08_video_scale",
		.default_value = "2x"
	},
	{
		.key = "fake08_crop_h_left",
		.desc = "Crop Horiz. Left"
	},
	{
		.key = "fake08_crop_h_right",
		.desc = "Crop Horiz. Right"
	},
	{
		.key = "fake08_crop_v_top",
		.desc = "Crop Vert. Top"
	},
	{
		.key = "fake08_crop_v_bottom",
		.desc = "Crop Vert. Bottom"
	},
};


me_bind_action fake08_ctrl_actions[] =
{
	{ "UP       ",  1 << RETRO_DEVICE_ID_JOYPAD_UP},
	{ "DOWN     ",  1 << RETRO_DEVICE_ID_JOYPAD_DOWN },
	{ "LEFT     ",  1 << RETRO_DEVICE_ID_JOYPAD_LEFT },
	{ "RIGHT    ",  1 << RETRO_DEVICE_ID_JOYPAD_RIGHT },
	{ "O        ",  1 << RETRO_DEVICE_ID_JOYPAD_A },
	{ "X        ",  1 << RETRO_DEVICE_ID_JOYPAD_B },
	{ "START    ",  1 << RETRO_DEVICE_ID_JOYPAD_START },
	{ NULL,       0 }
};

#define fake08_overrides {                              \
	.core_name = "fake08",                                \
	.actions = fake08_ctrl_actions,                       \
	.action_size = array_size(fake08_ctrl_actions),       \
	.options = fake08_core_option_overrides,              \
	.defer_frames = 3,                                    \
}
