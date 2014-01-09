#include	"driver.h"

char input[] = "p+p+p";

char start = 'E';

void
grammar(void) {
	rule('E', "E+T");
	rule('E', "T");

	rule('T', "T*F");
	rule('T', "F");

	rule('F', "F^p");
	rule('F', "p");
}

