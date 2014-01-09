/*	Note 1:
	We do not apply the Warshall algorithm to the grammar; the full
	Do"mo"lki algorithm is quite capable of handling single length
	productions.

	Note 2:
	This is essentially a non-deterministic BC parser.
	What Hext and Roberts call a parse, we would call a reduction.
*/

#include	<stdio.h>
#include	<string.h>

#include	"driver.h"
#include	"hardware.h"
#include	"parser.h"
#include	"print.h"

#define		DEBUG

struct stack_entry stack[MAXSTACK];
int p;						/* stack pointer */
static int i;					/* input pointer */

struct vector U, V;
static int pn[MACHINE_WIDTH];

static int length[MAXRULE];
static char subject[MAXRULE];

#ifdef	DEBUG
#define	debugf(f,v)	printf(f, v)
#define	debug_vec(n,v)	print_vec(n,v)
#else
#define	debugf(f,v)
#define	debug_vec(n,v)
#endif

void
add_rule(char lhs, const char *rhs) {
	static int rule_cnt;
	int rhs_ln = strlen(rhs);

	rule_cnt++;
	if (rule_cnt == MAXRULE)
		error("Too many rules");
	if (rhs_ln == 0)
		error("Empty right hand side");
	subject[rule_cnt] = lhs;
	length[rule_cnt] = rhs_ln;
	if (G_size+rhs_ln >= MACHINE_WIDTH)
		error("Grammar too long");
	strcpy(&G[G_size], rhs);
	U.bit[G_size] = 1;
	G_size += rhs_ln;
	pn[G_size-1] = rule_cnt;
	V.bit[G_size-1] = 1;
}

void
algorithm_B(int exhaustive) {
	char C;
	int pi, link, r;
	struct vector Q;

	i = -1;					/* UNIX strings start at 0 */
	p = 0;
	Q = zero;
	stack[0].pi = -1;
AA:	/* input one token */
	i = i + 1;
	C = input[i];
	debugf("AA: input %c\n", C);
	pi = 0;
	link = p;
BB:	/* process one token, from input or parse */
	debugf("BB: C = %c\n", C);
	debug_vec("Q", Q);
	Q = and(or(rsh(Q), U), Mstar(C));	/* the Domolki statement */
	debug_vec("Q", Q);
	p = p + 1;
	if (p == MAXSTACK)
		error("Stack overflow");
	stack[p].C = C;
	stack[p].Q = Q;
	stack[p].i = i;
	stack[p].pi = pi;
	stack[p].link = link;
CC:	/* examine the result, from processing or backtracking */
	if (is_zero(Q))
		goto backtrack;
	if (is_zero(and(Q, V)))
		goto AA;

/* now we have completed a parse */
	pi = pn[first_in(and(Q, V))];
	r = length[pi];
	C = subject[pi];
	debugf("completed rule %d", pi);
	debugf(", length = %d", r);
	debugf(", subject = %c\n", C);
	/* find beginning of the recognized rule on the stack */
	link = p;
	while (r) {
		link = stack[link].link;
		r = r - 1;
	}
	Q = stack[link].Q;
	if (C == start && link == 0 && input[i+1] == '\0')
		goto parse_found;
	else
		goto BB;

backtrack:
	/* find the latest parse */
	while (stack[p].pi == 0) {
		p = p - 1;
	}
	if (p == 0) goto no_parse_possible;
	/* and remove it */
	p = p - 1;
	Q = stack[p].Q;
	i = stack[p].i;
	Q = less_its_first_1bit_that_matches(Q, V);
	stack[p].Q = Q;
	debugf("backtracked to %d\n", p);
	goto CC;

parse_found:
	printf("OK, pi = %d\n", pi);
	print_stack("parse_found");
	if (exhaustive)
		goto BB;
	else
		return;

no_parse_possible:
	if (!exhaustive)
		printf("KO\n");
	return;
}
