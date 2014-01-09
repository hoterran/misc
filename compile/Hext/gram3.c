#include	"driver.h"

char input[] = "xxxxxx";

char start = 'S';

void
grammar(void) {
	rule('S', "A");

	rule('A', "AA");
	rule('A', "x");
}

