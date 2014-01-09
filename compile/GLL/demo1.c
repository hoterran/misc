/* demo grammar 1 from chapter 11 */

#include	"parser.h"

const char *input[] = {"ab", 0};

const struct rule grammar[] = {
	{'S', "AB"},
	{'A', "a"},
	{'A', "X"},
	{'B', "b"},
	{'X', "a"},
	{0, 0}
};

