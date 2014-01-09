/*	Some printing routines */

#include	<stdio.h>

#include	"hardware.h"
#include	"parser.h"
#include	"print.h"

void
print_grammar(void) {
	printf("G: %s\n", G);
	print_vec("U", U);
	print_vec("V", V);
}

void
print_vec(const char *m, struct vector v) {
	int j;

	printf("%s: ", m);
	for (j = 0; j < G_size; j++) {
		putchar(v.bit[j] ? '1' : '0');
	}
	printf("\n");
}

void
print_stack(const char *s) {
	int q;

	printf("      C   i pi link Q, at %s\n", s);
	for (q = 0; q <= p; q++) {
		int j;
		struct stack_entry *se = &stack[q];

		printf("[%2d] ", q);
		printf("'%c' ", se->C ? se->C : ' ');
		printf("%2d ", se->i);
		printf("%2d ", se->pi);
		printf("%2d ", se->link);
		printf("  ");
		for (j = 0; j < G_size; j++) {
			putchar(se->Q.bit[j] ? '1' : '0');
		}
		printf("\n");
	}
}

