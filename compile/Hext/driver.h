
#define	MAXSTACK	400
#define	MAXRULE		20

extern char input[];
extern char start;
extern void grammar(void);

extern void rule(char lhs, const char *rhs);
extern void error(const char *s);
