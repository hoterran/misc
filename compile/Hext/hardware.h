/*	This is the hardware for the Do"mo"lki machine */

#define	MACHINE_WIDTH  	60		/* width of the grammar registers */

struct vector {
	char bit[MACHINE_WIDTH];
};

extern struct vector zero;

extern struct vector and(struct vector v1, struct vector v2);
extern struct vector or(struct vector v1, struct vector v2);
extern int is_zero(struct vector v);
extern struct vector rsh(struct vector v);
extern int first_in(struct vector v);
extern struct vector less_its_first_1bit_that_matches
    (struct vector v1, struct vector v2);
extern struct vector Mstar(char c);

extern char G[MACHINE_WIDTH];			/* the grammar */
extern int G_size;

