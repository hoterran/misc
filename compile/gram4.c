#include		"global.h"

/*	E ::= E [+ -] T
	E ::= T
	T ::= T [* /] F
	T ::= F
	F ::= [xyz]
	F ::= ( E )
*/

char start = 'S';

void
grammar(void) {
	rule('S', "E");

	rule('E', "E+T");		/* left-recursive, no LL */
	rule('E', "E-T");
	rule('E', "T");

	rule('T', "T*F");
	rule('T', "T/F");
	rule('T', "F");

	rule('F', "x");
	rule('F', "y");
	rule('F', "z");
	rule('F', "(E)");
}

