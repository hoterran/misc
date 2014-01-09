struct rule {
	const char lhs;
	const char *rhs;
};

extern const char *input[];
extern const struct rule grammar[];

/* the stack elements */
struct elem {
	char token;
	int pos;			/* where created / prediction count */
	int is_expanded;
	struct elem *parent;
	struct backptr *backptr;	/* pointer to list */
	int is_nullable;
	/* local field for determining infinite recursion: */
	int _scan_count;
	/* local field for printing purposes only: */
	int _number;
	int _print_count;
};

struct backptr {
	struct elem *elem;
	struct backptr *next;
};

/* the top of the stack */
struct top {
	struct elem *elem;
	struct top *next;
};

#define	new(type)	((type *) malloc(sizeof (type)))

extern int trace;
extern int debug;
extern int no_LL;

