/*	This is a simple emulation of the hardware of the Do"mo"lki machine */

#include	"hardware.h"

struct vector zero;

char G[MACHINE_WIDTH];
int G_size;

struct vector
and(struct vector v1, struct vector v2) {
	int j;

	for (j = 0; j < G_size; j++)
		v1.bit[j] &= v2.bit[j];
	return v1;
}

struct vector
or(struct vector v1, struct vector v2) {
	int j;

	for (j = 0; j < G_size; j++)
		v1.bit[j] |= v2.bit[j];
	return v1;
}

int
is_zero(struct vector v) {
	int j;

	for (j = 0; j < G_size; j++)
		if (v.bit[j])
			return 0;
	return 1;
}

struct vector
rsh(struct vector v) {
	int j;
	char carry = 0;

	for (j = 0; j < G_size; j++) {
		char bit = v.bit[j] ;
		v.bit[j] = carry;
		carry = bit;
	}
	return v;
}

int
first_in(struct vector v) {
	int j;

	for (j = 0; j < G_size; j++) {
		if (v.bit[j])
			break;
	}
	return j;
}

struct vector
less_its_first_1bit_that_matches(struct vector v1, struct vector v2) {
	int j;

	for (j = 0; j < G_size; j++) {
		if (v1.bit[j] & v2.bit[j]) {
			v1.bit[j] = 0;
			break;
		}
	}
	return v1;
}

struct vector
Mstar(char c) {
	int j;
	struct vector v;

	v = zero;
	for (j = 0; j < G_size; j++) {
		if (G[j] == c) {
			v.bit[j] = 1;
		}
	}
	return v;
}

