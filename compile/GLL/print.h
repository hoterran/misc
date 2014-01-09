/*
  Prints the prediction stacks with loops.
  Common parts of a prediction are shown as ... after the first node that has
  already been printed.
  To identify nodes they have been given identification numbers.
  Back pointers from a node are given between { and }.
  Since the loops are not reachable from the usual prediction top set, they
  are collected and printed separately, marked loop:.
*/

extern void print_stacks(const char *msg, struct top *tp);
extern void print_elem(const struct elem *e);
