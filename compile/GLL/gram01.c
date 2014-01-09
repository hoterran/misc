/* very non-LL prediction problem */

#include	"parser.h"

const char *input[] = {"", "(((])", "(((])]", "(((]])]", 0};

const struct rule grammar[] = {
	{'S', "(S)"},
	{'S', "(S]"},
	{'S', ""},
	{0, 0}
};

