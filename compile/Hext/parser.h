struct stack_entry {
	char C;
	struct vector Q;
	int i;
	int pi;
	int link;
};


extern struct stack_entry stack[];
extern int p;					/* stack pointer */

extern struct vector U, V;

extern void add_rule(char lhs, const char *rhs);
extern void algorithm_B(int exhaustive);
