/*	This is the context-sensitive parser described in:
	William A. Woods, Context-sensitive parsing,
	Commun. ACM, Vol. 13, #7, July 1970, 437-445.
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"global.h"

#define	MAXRULES	50
#define	RHSLENGTH	10
#define	INPUTSIZE	50
#define	STACKSIZE	256

struct rule {
	char lhs;
	char rhs[RHSLENGTH+1];
	char context[RHSLENGTH+1];
};

static struct rule rule_list[MAXRULES];
static int nrules;

struct symbol {
	char symbol;
	int mat_level;
	int birthday;
};

struct working_stack {
	struct symbol symbols[INPUTSIZE];
};

struct stack_entry {
	struct working_stack ws;
	int ws_size;
	int i;
	int r;
	int l;
};	

static struct working_stack ws;
static int ws_size;
static struct stack_entry pd_stack[STACKSIZE];	/* the push-down stack */
static int SP;

#define	max(a,b)	((a) > (b) ? (a) : (b))

static int i;				/* marks scanned position in the ws */
static int r;				/* marks position in rule list */
static int l;				/* parser level */

static void
error(const char *msg) {
	printf("ERROR: %s\n", msg);
	exit(1);
}

static int
match(int *a, int *b, int *c, int *d) {
	struct rule *rl = &rule_list[r];
	int i1 = i;
	char *cp;
	
	*a = i1;
	for (cp = rl->context; *cp; cp++) {
		if (i1 > ws_size)
			return 0;
		if (*cp != '_') {
			/* match the context */
			if (ws.symbols[i1].symbol != *cp)
				return 0;
			i1++;
		}
		else {
			/* ascertain presence of constituents */
			char *rhp;

			*b = i1;
			for (rhp = rl->rhs; *rhp; rhp++) {
				if (i1 > ws_size)
					return 0;
				/* match one constituent */
				if (ws.symbols[i1].symbol != *rhp)
					return 0;
				i1++;
			}
			*c = i1;
		}
	}
	*d = i1;

	return 1;
}

static int
red_lev(int a, int b, int c, int d) {
	int redlev = 0;
	int j;

	for (j = a; j < b; j++) {
		redlev = max(redlev, ws.symbols[j].birthday);
	}
	for (j = b; j < c; j++) {
		redlev = max(redlev, ws.symbols[j].mat_level);
	}
	for (j = c; j < d; j++) {
		redlev = max(redlev, ws.symbols[j].birthday);
	}
	return redlev;
}

static void
reduce(	int a, int b, int c, int d) {
	int j;

	for (j = a; j < b; j++) {
		ws.symbols[j].mat_level = l+1;
	}
	for (j = b; j == b; j++) {
		ws.symbols[j].symbol = rule_list[r].lhs;
		ws.symbols[j].birthday = l+1;
		ws.symbols[j].mat_level = l+1;
	}
	for (j = c; j < d; j++) {
		ws.symbols[j].mat_level = l+1;
	}
	for (j = c; j <= ws_size; j++) {
		ws.symbols[j-(c-b-1)] = ws.symbols[j];
	}
	ws_size -= (c-b-1);
}

static void
push_ws(void) {
	struct stack_entry *se = &pd_stack[SP++];

	if (SP > STACKSIZE)
		error("Stack overflow");
	se->ws = ws;
	se->ws_size = ws_size;
	se->i = i;
	se->r = r;
	se->l = l;
}

static void
pop_ws(void) {
	struct stack_entry *se = &pd_stack[--SP];

	ws = se->ws;
	ws_size = se->ws_size;
	i = se->i;
	r = se->r;
	l = se->l;
}

static void
print_stack(void) {
	int sp;

	for (sp = 0; sp < SP; sp++) {
		struct stack_entry *se = &pd_stack[sp];
		int j;

		printf("sp=%2d, i=%2d, r=%2d, l=%2d, ",
			sp, se->i, se->r, se->l);
		for (j = 1; j <= se->ws_size; j++) {
			struct symbol *sb = &se->ws.symbols[j];

			printf("[%d%c%d]",
				sb->birthday, sb->symbol, sb->mat_level);
		}
		printf("\n");
	}
	printf("\n");
}

static void
print_deriv(void) {
	printf("OK, SP = %d\n", SP);
	print_stack();
}

static void
print_grammar(void) {
	int j;

	for (j = 1; j <= nrules; j++) {
		struct rule *rl = &rule_list[j];
		int k;

		printf("[%d] %c -> %s/%s", j, rl->lhs, rl->rhs, rl->context);
		printf("\t(");
		for (k = 0; rl->context[k]; k++) {
			if (rl->context[k] == '_')
				printf("%c", rl->lhs);
			else	printf("%c", rl->context[k]);
		}
		printf(" -> ");
		for (k = 0; rl->context[k]; k++) {
			if (rl->context[k] == '_')
				printf("%s", rl->rhs);
			else	printf("%c", rl->context[k]);
		}
		printf(")\n");
	}
}

static void
parse(void) {
	/* the context-sensitive parsing algorithm, page 439, op. cit. */
	int i1, r1, l1;
	int a, b, c, d;

	goto L1;
L1:
	i = 1, r = 1, l = 0;
L2:
	if (!match(&a, &b, &c, &d))
		goto L4;
	if (red_lev(a, b, c, d) == l)
		goto L3;
	goto L4;
L3:
	push_ws();
	reduce(a, b, c, d);
	i = 1, r = 1, l = l+1;
	goto L2;
L4:
	r = r+1;
	if (r <= nrules)
		goto L2;
	i = i+1, r = 1;
	if (i <= ws_size)
		goto L2;
	goto L5;
L5:
	if (SP == 0)
		return;
	i1 = pd_stack[SP-1].i;
	r1 = pd_stack[SP-1].r;
	l1 = pd_stack[SP-1].l;
	if (l1 < l)
		goto L6;
	goto L7;
L6:
	if (ws_size == 1 && ws.symbols[1].symbol == start)
		print_deriv();
	i = i1, r = r1, l= l1;
	goto L4;
L7:
	pop_ws();
	i = i1, r = r1, l= l1;
	goto L4;
}

int
main(void) {
	int i;

	grammar();
	print_grammar();

	/* fill ws */
	for (i = 0; input[i]; i++) {
		struct symbol *sb = &ws.symbols[++ws_size];

		if (ws_size == INPUTSIZE)
			error("Input too long");
		sb->symbol = input[i];
		sb->mat_level = 0;
		sb->birthday = 0;
	}

	parse();

	return 0;
}

void
rule(char lhs, char *rhs, char *context) {
	struct rule *rl = &rule_list[++nrules];

	if (nrules == MAXRULES)
		error("Too many rules");
	rl->lhs = lhs;
	if (strlen(rhs) >= RHSLENGTH)
		error("Rhs too long");
	strcpy(rl->rhs, rhs);
	if (strlen(context) >= RHSLENGTH)
		error("Context too long");
	strcpy(rl->context, context);
}

