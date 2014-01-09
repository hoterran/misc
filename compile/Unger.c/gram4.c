#include	"TD_parser.h"
#include	"grammar.h"

void InitGrammar(void) {
	StoreStartSymbol('S');
	StoreRule('S', "LSR");
	StoreRule('S', "");
	StoreRule('L', "(");
	StoreRule('L', "");
	StoreRule('R', ")");
}

void DoParses(void) {
	Parse("())");
	Parse("((()))))");
}
