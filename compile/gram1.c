#include	"global.h"

char start = 'A';

void
grammar(void) {
	rule('A', "(B)");
	rule('A', "a");
	rule('B', "(C)");
	rule('B', "");
	rule('C', "c");
}

