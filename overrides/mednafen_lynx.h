#include "overrides.h"

static const struct core_override_option mednafen_lynx_core_option_overrides[] = {
	{
		.key = "lynx_rot_screen",
		.blocked = true
	},
	{
		.key = "lynx_pix_format",
		.desc = "Color Format",
		.info = "Change color format between 16-bit (RGB565) or 32-bit (RGB8888). Restart required.",
		.options = {
			{ "16", "16-bit" },
			{ "32", "32-bit" },
		}
	},
	{
		.key = "lynx_force_60hz",
		.default_value = "enabled"
	},
};

#define mednafen_lynx_overrides {                            \
	.core_name = "mednafen_lynx",                            \
	.options = mednafen_lynx_core_option_overrides           \
}
