/* test creation of expansion-marked nodes */

#include	"parser.h"

const char *input[] = {"", "ag", 0};

const struct rule grammar[] = {
	{'S', "BA"},
	{'B', "a"},
	{'B', "U"},
	{'U', ""},
	{'A', "g"},
	{'A', "h"},
	{0, 0}
};

