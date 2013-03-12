%{
#include "fb1-5.tab.h"
%}

%%

"(" {return OP;}
")" {return CP;}

"+"	{ return ADD; }
"-"	{ return SUB; }
"*"	{ return MUL; }
"/"	{ return DIV; }
"|"	{ return ABS; }
[0-9]+  { yylval = atoi(yytext); return NUMBER; }
\n	{ return EOL; }
[ \t]   { }
.	{ printf("Mystery %s", yytext); }

%%

int yywrap() {
	return 0;
}
