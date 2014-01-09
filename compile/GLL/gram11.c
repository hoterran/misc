/* left-recursion fest */

#include	"parser.h"

const char *input[] = {"", "a", "aa", 0};

const struct rule grammar[] = {
	{'S', "STU"},
	{'T', "TUS"},
	{'U', "UST"},
	{'S', ""},
	{'T', ""},
	{'U', ""},
	{'S', "a"},
	{0, 0}
};

