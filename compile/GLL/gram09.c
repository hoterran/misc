/* non-productive grammar */

#include	"parser.h"

const char *input[] = {"", "x", "xx", 0};

const struct rule grammar[] = {
	{'S', "xA"},
	{'A', "B"},
	{'B', "C"},
	{'B', "S"},
	{'C', "A"},
	{0, 0}
};

