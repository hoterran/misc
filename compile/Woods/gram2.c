/*	Woods' own grammar for a^n b^n c^n */

#include	"global.h"

char input[] = "aaaabbbbcccc";

char start = 'S';

void
grammar(void) {
	rule('S', "ASCB",	"_");
	rule('S', "ACB",	"_");
	rule('B', "R",		"C_");
	rule('C', "B",		"_R");
	rule('R', "C",		"B_");
	rule('B', "b",		"A_");
	rule('A', "a",		"_b");
	rule('A', "a",		"_a");
	rule('B', "b",		"b_");
	rule('C', "c",		"b_");
	rule('C', "c",		"c_");
}

