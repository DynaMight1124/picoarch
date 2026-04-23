#include "overrides.h"

static const struct core_override_option fbneo_core_option_overrides[] = {
	{
		.key = "fbneo-vertical-mode",
		.blocked = true
	},
};

#define fbneo_overrides {                           \
	.core_name = "fbneo",                             \
	.options = fbneo_core_option_overrides,           \
}
