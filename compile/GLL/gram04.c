/* indirect left recursion */

#include	"parser.h"

const char *input[] = {"x", "xx", "xxx", "xxxx", "xxxxx", 0};

const struct rule grammar[] = {
	{'S', "Tx"},
	{'T', "Ux"},
	{'U', "Sx"},
	{'T', ""},
	{0, 0}
};

