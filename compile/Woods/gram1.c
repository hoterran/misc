#include	"global.h"

char input[] = "aaabbbccc";

char start = 'S';

void
grammar(void) {
	rule('S', "aSQ", "_");
	rule('S', "abc", "_");
	rule('Q', "bc", "b_c");
	rule('Q', "X", "c_");
	rule('c', "Q", "_X");
	rule('X', "c", "Q_");
}

