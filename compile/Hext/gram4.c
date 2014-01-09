/*	E ::= E [+|-] T
	E ::= T
	T ::= T [*|/] F
	T ::= F
	F ::= [xyz]
	F ::= ( E )
*/

#include	"driver.h"

char input[] = "x-y*z+(x+z/y)*y";

char start = 'E';

void
grammar(void) {
	rule('E', "E+T");
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
