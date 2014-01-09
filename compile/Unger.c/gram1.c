/*	This is the LL(inf) grammar
		S -> A
		A -> (A] | B 
		B -> (B) | eps
*/

#include	"TD_parser.h"
#include	"grammar.h"

void InitGrammar(void) {
	StoreStartSymbol('S');
	StoreRule('S', "A");
	StoreRule('A', "(A]");
	StoreRule('A', "B");
	StoreRule('B', "(B)");
	StoreRule('B', "");
}

void DoParses(void) {
	Parse("((()]]");
	Parse("");
}
