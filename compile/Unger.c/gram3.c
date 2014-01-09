/*	This grammar requires the prediction of a number of empty strings
	depending on the number of a"s:
		S -> LSa | eps
		L -> eps
*/

#include	"TD_parser.h"
#include	"grammar.h"

void InitGrammar(void) {
	StoreStartSymbol('S');
	StoreRule('S', "LSa");
	StoreRule('S', "");
	StoreRule('L', "");
}

void DoParses(void) {
	Parse("");
	Parse("aaaa");
}
