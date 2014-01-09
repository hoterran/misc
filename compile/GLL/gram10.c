/* left-recursion with unit and eps rules */

#include	"parser.h"

const char *input[] = {"", "a", "aa", "abab", 0};

const struct rule grammar[] = {
	{'S', ""},
	{'S', "S"},
	{'S', "TSa"},
	{'S', "STb"},
	{'T', ""},
	{0, 0}
};

