/* finding the middle of a string */

#include	"parser.h"

const char *input[] = {"x", "xx", "xxx", "xxxx", "xxxxx", 0};

const struct rule grammar[] = {
	{'S', "xSx"},
	{'S', "x"},
	{0, 0}
};

