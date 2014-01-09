/* hidden indirect left recursion */

#include	"parser.h"

const char *input[] = {"x", "xx", "xxx", "xxxx", "xxxxx", 0};

const struct rule grammar[] = {
	{'S', "ATx"},
	{'T', "BUx"},
	{'U', "CSx"},
	{'T', ""},
	{'A', "a"},
	{'A', ""},
	{'B', "B"},
	{'B', ""},
	{'C', "C"},
	{'C', ""},
	{0, 0}
};

