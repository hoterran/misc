/* infinite ambiguity */

#include	"parser.h"

const char *input[] = {"", "x", 0};

const struct rule grammar[] = {
	{'S', "Ax"},
	{'A', "B"},
	{'B', "C"},
	{'B', ""},
	{'C', "A"},
	{0, 0}
};

