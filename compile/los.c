/*	produces the longest original sentence in a CF language
*/

#include	<stdio.h>
#include	<string.h>

#include	"global.h"
#include	"st_char.h"

static struct st_char stack[STACKSIZE];
static int SP;

static char text[TEXTSIZE];
static int TP;

static int active[256];

static int
is_non_terminal(char tk) {
	return 'A' <= tk && tk <= 'Z';
}

static void
generate(void) {
	/* one step of the non-deterministic pushdown automaton */
/*	print_status();*/

	if (SP == 0) {
		/* sentence found */
		text[TP] = 0;
		printf("%s\n", text);
	}
	else
	/* end of rhs? */
	if (stack[SP-1].marker) {
		char nt;

		/* remove the marker */
		nt = stack[--SP].ch;

		active[nt]--;

		generate();

		/* restore the marker */
		active[nt]++;
		stack[SP++].ch = nt;
		stack[SP-1].marker = 1;
	}
	else
	/* try to generate one symbol */
	if (!is_non_terminal(stack[SP-1].ch)) {
		/* shift it */
		text[TP++] = stack[--SP].ch;

		generate();

		/* shift it back */
		stack[SP++].ch = text[--TP];
		stack[SP-1].marker = 0;
	}
	else {
		/* try to expand one lhs */
		grammar();
	}
}

void
rule(char lhs, const char rhs[]) {
	int rhs_len = strlen(rhs);

	if (active[lhs] <= 1 && stack[SP-1].ch == lhs) {
		/* the rule applies */
		int i;

		/* make lhs a marker */
		stack[SP-1].marker = 1;
		active[lhs]++;

		/* push rhs, reversed */
		for (i = rhs_len; i > 0; i--) {
			stack[SP++].ch = rhs[i-1];
			stack[SP-1].marker = 0;
		}

		generate();

		/* pop rhs */
		SP -= rhs_len;
		
		/* push lhs */
		stack[SP-1].marker = 0;
		active[lhs]--;
	}
}

static void
print_status(void) {
	int i;

	printf("t: ");
	for (i = 0; i < TP; i++)
		putchar(text[i]);
	printf("\n");

	printf("s: ");
	for (i = SP; i > 0; i--) {
		putchar(stack[i-1].ch);
		putchar(stack[i-1].marker ? '\'' : ' ');
	}
	printf("\n");
}

int
main(void) {
	/* prepare stack */
	stack[SP++].ch = start;		/* stack := start */
	stack[SP-1].marker = 0;

	generate();
	return 0;
}

