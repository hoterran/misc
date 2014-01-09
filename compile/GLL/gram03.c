/* simple left recursion */

#include	"parser.h"

const char *input[] = {"", "x", "xxxx", "xxxxxxxxxx", 0};

const struct rule grammar[] = {
	{'S', "Sx"},
	{'S', "x"},
	{0, 0}
};

