#include	<malloc.h>
#include	<string.h>

#include	"parser.h"
#include	"LLtables.h"

static int n_rules;
static char *nullable;				/* set of ... */
static char *non_terminals;			/* set of ... */

static int
count_rules(void) {
	int i = 0;
	const struct rule *r;
	
	for (r = grammar; r->lhs; r++) {
		i++;
	}

	return i;
}

static void
fill_non_terminals(void) {
	int i = 0;
	const struct rule *r;

	non_terminals[i] = '\0';
	for (r = grammar; r->lhs; r++) {
		if (!is_non_terminal(r->lhs)) {
			non_terminals[i++] = r->lhs;
			non_terminals[i] = '\0';
		}
	}
}

int
is_non_terminal(char n) {
	return strchr(non_terminals, n) != 0;
}

static int
is_certainly_nullable(const char *rhs) {
	const char *p;
	
	for (p = rhs; *p; p++) {
		if (!is_nullable(*p)) {
			return 0;
		}
	}
	return 1;
}

static void
fill_nullable(void) {
	int i = 0;
	int done = 0;

	nullable[i] = '\0';
	while (!done) {
		const struct rule *r;
	
		done = 1;		
		for (r = grammar; r->lhs; r++) {
			if (	!is_nullable(r->lhs)
			&&	is_certainly_nullable(r->rhs)
			) {
				nullable[i++] = r->lhs;
				nullable[i] = '\0';
				done = 0;
			}
		}
	}
}

int
is_nullable(char n) {
	return strchr(nullable, n) != 0;
}

void
compute_LL_tables(void) {
	n_rules = count_rules();

	non_terminals = (char *)malloc((n_rules+1 ) * sizeof(char));
	fill_non_terminals();

	nullable = (char *)malloc((n_rules+1 ) * sizeof(char));
	fill_nullable();
}

const struct rule **
LL_table_entry(char lhs, char look_ahead) {
	/* this is a very simple routine to produce a sloppy LL(1) table entry:
	   any rule that does not directly contradict lhs and look_ahead is OK
	*/
	int i = 0;
	const struct rule *r;
	
	const struct rule **table_entry =
	    (const struct rule **)malloc((n_rules+1 ) * sizeof(struct rule *));

	for (r = grammar; r->lhs; r++) {
		char first = r->rhs[0];

		if (	/* the rule applies */
			r->lhs == lhs
		&&	/* does not start with a terminal != look_ahead */
			(	no_LL
			||	is_non_terminal(first)
			||	first == '\0'
			||	first == look_ahead
			)
		) {
			/* we accept it */
			table_entry[i++] = r;
		}
	}
	table_entry[i++] = 0;

	return table_entry;
}

