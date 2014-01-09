#include	<stdio.h>
#include	<malloc.h>

#include	"parser.h"
#include	"print.h"

static int print_count;
static struct top *loops;

static void
print_token(const struct elem *e) {
	printf("%c_%d", e->token, e->_number);
}

void
print_elem(const struct elem *e) {
	const struct backptr *bp;

	if (e == 0) {
		printf("NULL");
		return;
	}
	
	printf(e->is_expanded ? "[" : "");
	print_token(e);
	printf(e->is_expanded ? "]" : "");
	
	bp = e->backptr;
	if (bp) {
		printf("{");
		while (bp) {
			print_token(bp->elem);
			if (bp->next) {
				printf(",");
			}
			if (	/* there is another node to print */
				bp->next
			&&	/* this one has already been printed */
				bp->elem->_print_count == print_count
			) {
				printf("...");
				break;
			}
			if (bp->elem->_print_count != print_count) {
				/* the backptr has not yet been printed;
				   make sure it does
				*/
				struct top *t = new(struct top);
				t->elem = bp->elem;
				t->next = loops;
				loops = t;
			}
			bp = bp->next;
		}
		printf("}");
	}
	printf(" ");
}

static void
print_stack(struct elem *e) {

	if (e) {
		while (e) {
			print_elem(e);
			if (	/* there is another node to print */
				e->parent
			&&	/* this one has already been printed */
				e->_print_count == print_count
			) {
				printf(" ...");
				break;
			}
			e->_print_count = print_count;
			e = e->parent;
			
		}
	}
	else {
		printf("[empty prediction]");
	}
	printf("\n");
}		

void
print_stacks(const char *msg, struct top *tp) {

	if (!trace && !debug) return;

	printf("%s:\n", msg);
	print_count++;
	loops = 0;

	while (tp) {
		print_stack(tp->elem);
		tp = tp->next;
	}

	tp = loops;
	while (tp) {
		if (tp->elem->_print_count != print_count) {
			printf("loop: ");
			print_stack(tp->elem);
			/* kludge: start loops over again, since elements may
			   have been added
			*/
			tp = loops;
		}
		tp = tp->next;
	}
}

