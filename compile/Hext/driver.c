/*	This is an implementation of the Do"mo"lki algorithm as described in
	J.B. Hext, P.S. Roberts, Syntax analysis by Dom\(:olki's algorithm,
	Computer J., Vol. 13, 3, Aug 1970, 263-271. 
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"driver.h"
#include	"hardware.h"
#include	"parser.h"
#include	"print.h"

void
error(const char *s) {
	printf("ERROR: %s\n", s);
	exit(1);
}

void
rule(char lhs, const char *rhs) {
	add_rule(lhs, rhs);
}


int
main(void) {
	grammar();
	print_grammar();
	algorithm_B(0);
	return 0;
}

