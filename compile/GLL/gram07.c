/* sequence of 3 palindromes */

#include	"parser.h"

const char *input[] = {"aa", "aabbaabb", "abbaaabbbb", 0};

const struct rule grammar[] = {
	{'S', "PPP"},
	{'P', "aPa"},
	{'P', "bPb"},
	{'P', ""},

	{0, 0}
};

