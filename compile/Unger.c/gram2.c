/*	This is the highly ambiguous grammar
		S -> A
		A -> AA | a | eps
*/

#include	"TD_parser.h"
#include	"grammar.h"

void InitGrammar(void) {
	StoreStartSymbol('S');
	StoreRule('S', "A");
	StoreRule('A', "AA");
	StoreRule('A', "");
	StoreRule('A', "a");
}

void DoParses(void) {
	Parse("aaaa");
	Parse("");
}
