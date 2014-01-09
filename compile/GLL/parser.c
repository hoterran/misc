/* Generalized LL parser from van Deudekom and Kooiman,
   Top-Down Non-Correcting Error Recovery in LLgen;
   extended to handle infinite ambiguity (under breadth-first prediction only)
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"parser.h"
#include	"LLtables.h"
#include	"print.h"

/* Modes of operations */
static char predict_mode = 'B';
/* B = breadth-first;
   D = depth-first, fails for present implementation of infinite ambiguity.
*/

int trace = 0;		/* trace before, during and after predicting */
int debug = 0;		/* produce additional debug info */
int no_LL = 0;		/* don't use any LL information */

							/* THE STACK */
static int ip;				/* input pointer */
static int elem_number = 0;		/* to identify a node in printing */

static struct elem *
new_elem(char token, struct elem *parent) {
	struct elem *e = new(struct elem);

	e->token = token;
	e->pos = ip;
	e->is_expanded = 0;
	e->is_nullable = is_nullable(token);	/* info from LL(1) parser */
	e->parent = parent;
	e->backptr = 0;
	/* local fields */
	e->_scan_count = 0;
	e->_number = elem_number++;
	e->_print_count = 0;

	return e;
}

static struct top *Top;
static struct top **Top_hook;

static void
init_Top(void) {
	Top = 0;
	Top_hook = &Top;
}

							/* PREDICTION */
static void add_to_Top(struct elem *e);

static void
init_stack(void) {
	add_to_Top(new_elem(grammar[0].lhs, 0));
}

static int
in_present_prediction(const struct elem *e) {
	return e->pos == ip;
	/* vD&K use a set to implement in_present_prediction and use that set
	   for further optimizations; TO DO?
	*/
}

static struct elem *
expanding_in_present_prediction(char lhs, struct elem *e) {
	while (e) {
		if (	in_present_prediction(e)
		&&	e->token == lhs
		&&	e->is_expanded
		) return e;
		e = e->parent;
	}
	return 0;
}

static struct elem *
push_rhs(const char *rhs, struct elem *base) {
	int i;

	for (i = strlen(rhs) - 1; i >= 0; i--) {
		base = new_elem(rhs[i], base);
	}
	return base;
}

static scan_count = 0;	/* increased for every scan over the pred. graph */
static int
detailed_properties_by_scan(const struct elem *e, struct elem *from);

static const int InfinitelyAmbiguous = 1;
/* the desired node has been found in the nullable prediction graph, so it is
   infinitely ambiguous
*/
static const int AlreadyInTop = 2;
/* a node with the same name as the desired node has been found in the
   prediction graph AND in the Top, so its has been / will be expanded
*/

static int
token_in_Top(char token) {
	const struct top *t = Top;

	while (t) {
		if (t->elem && t->elem->token == token) return 1;
		t = t->next;
	}
	return 0;
}

static int
detailed_properties(const struct elem *e, struct elem *from) {
	/* assemble answers ... */
	int res = 0;

	if (from == 0) return 0;

	if (from->_scan_count == scan_count) return 0;	/* been here */
	from->_scan_count = scan_count;

	if (from == e) {
		res |= InfinitelyAmbiguous;
	}

	if (token_in_Top(from->token)) {
		res |= AlreadyInTop;
	}

	if (!from->is_expanded && !from->is_nullable) return 0;/* no passage */

	/* ... and search on */
	return detailed_properties_by_scan(e, from) | res;
}

static int
detailed_properties_by_scan(const struct elem *e, struct elem *from) {
	const struct backptr *b = from->backptr;
	int res = detailed_properties(e, from->parent);

	while (b) {
		res |= detailed_properties(e, b->elem);
		b = b->next;
	}
	return res;
}

static int
prediction_properties(struct elem *e) {
	/* tests for the infinitely ambiguous occurrence of e in the
	   prediction graph and in Top
	*/
	scan_count++;

	return e ? detailed_properties_by_scan(e, e) : 0;
}

static void
add_to_Top(struct elem *e) {
	struct top *tp = new(struct top);

	if (debug) {
		printf("add_to_Top ");
		print_elem(e);
		printf("\n");
	}

	/* reject non-first occurrence of infinitely ambiguous element */
	if (prediction_properties(e) == (InfinitelyAmbiguous|AlreadyInTop))
		return;

	tp->elem = e;
	tp->next = 0;
	*Top_hook = tp;
	Top_hook = &tp->next;
}

static void
add_to_backptr_list(struct elem *e, struct elem *list_owner) {
	struct backptr *b = new(struct backptr);
	
	b->elem = e;
	b->next = list_owner->backptr;
	list_owner->backptr = b;
}

static void
expand(struct elem *e, char look_ahead) {
	struct elem *left_rec;

	if (e == 0) {
		/* empty prediction */
	}
	else
	if (!is_non_terminal(e->token)) {
		if (predict_mode == 'D') {
			add_to_Top(e);
		}
	}
	else
	if (e->is_expanded) {
		/* elem e recognized */
		/* we add its parent (which may be 0) to top */
		add_to_Top(e->parent);
		
		/* unless e is in the present prediction
		   we also add its backptrs to top
		*/
		if (!in_present_prediction(e)) {
			struct backptr *b = e->backptr;
			while (b) {
				add_to_Top(b->elem);
				b = b->next;
			}
		}
	}
	else
	if (left_rec = expanding_in_present_prediction(e->token, e)) {
		/* add e->parent, which is not 0, as a backpointer
		   to left_rec
		*/
		add_to_backptr_list(e->parent, left_rec);
		
		if (left_rec->is_nullable) {
			/* add it to the top as well */
			add_to_Top(e->parent);
		}
	}
	else {
		struct elem *base;
		const struct rule **entry;

		/* create marked copy and use it as base */
		base = new(struct elem);
		*base = *e;
		base->is_expanded = 1;
		base->_number = elem_number++;

		entry = LL_table_entry(base->token,look_ahead);

		while (*entry) {
			struct elem *new_top = push_rhs((*entry)->rhs, base);

			if (predict_mode == 'D') {
				expand(new_top, look_ahead);
			}
			else {
				add_to_Top(new_top);
			}
			entry++;
		}
	}
}

static void
predict(struct top *tp, char look_ahead) {
	/* predict for each top element */
	print_stacks("before predict", Top);
	while (tp) {
		if (tp->elem && is_non_terminal(tp->elem->token)) {
			expand(tp->elem, look_ahead);
			print_stacks("while predicting", Top);
		}
		tp = tp->next;
	}
	if (trace) print_stacks("after predict", Top);
}

							/* MATCHING */
static void
match(char token) {
	struct top *tp = Top;

	/* get a new top */
	init_Top();
	/* and fill it from the accepted tops of the old stacks */
	while (tp) {
		struct elem *e = tp->elem;

		if (e == 0) {
			/* no prediction, no match */
		}
		else
		if (e->token == token) {
			add_to_Top(e->parent);
		}
		tp = tp->next;
	}
	if (trace) print_stacks("after match", Top);
}

							/* THE PARSER */
static int
empty_prediction_in_Top(void) {
	const struct top *tp = Top;

	while (tp) {
		if (tp->elem == 0) return 1;
		tp = tp->next;
	}
	return 0;
}

static void
parse(const char *string) {
	elem_number = 0;
	ip = 0;
	init_Top();
	init_stack();

	for (;;) {
		if (Top) {
			predict(Top, string[ip]);
		}
		else {	/* no prediction left */
			/* the first time there is the prediction from the
			   start symbol, so now ip > 0 */
			printf("string `%s' not in L(G); ", string);
			printf("residue is `%s'\n", &string[ip-1]);
			break;
		}

		if (string[ip]) {
			match(string[ip]);
		}
		else {	/* string ended */
			if (empty_prediction_in_Top()) {
				printf("string `%s' in L(G)\n", string);
			}
			else {
				printf("premature end of string for `%s'\n",
				       string);
			}
			break;
		}
		
		ip++;
	}
}

int
main(int argc, char *argv[]) {
	int i;

	while (argc > 1 && argv[1][0] == '-') {
		char opt = argv[1][1];
		if (argv[1][2]) {
			fprintf(stderr, "no combined options allowed\n");
			exit (1);
		}
		switch (opt) {
		case 'B': case 'D':
			/* prediction mode: B=breadth-first, D=depth-first */
			predict_mode = argv[1][1];
			break;
		case 'L':
			/* no help from LL(1) table */
			no_LL = 1;
			break;
		case 't':
			trace = 1;
			break;
		case 'd':
			trace = 1; debug = 1;
			break;
		default:
			fprintf(stderr, "unknown option %s", argv[1]);
			exit (1);
		}
		argc--, argv++;
	}

	compute_LL_tables();	/* expected to be non-deterministic */

	for (i = 0; input[i]; i++) {
		parse(input[i]);
		printf("\n");
	}
	return 0;
}

