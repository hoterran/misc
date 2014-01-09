/* demo grammar 2 from chapter 11 */

#include	"parser.h"

const char *input[] = {"ab", 0};

const struct rule grammar[] = {
	{'S', "AB"},
	{'A', "a"},
	{'A', "S"},
	{'B', "b"},
	{0, 0}
};

