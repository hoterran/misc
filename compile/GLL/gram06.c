/* left-regular grammar */

#include	"parser.h"

const char *input[] = {"", "xba", "xbecxdc", 0};

const struct rule grammar[] = {
	{'S', "Aa"},
	{'S', "Cc"},
	{'S', ""},
	{'A', "Bb"},
	{'B', "Sx"},
	{'C', "Bd"},
	{'C', "Ae"},
	{0, 0}
};

