%option noyywrap 
%x COMMENT
%x OTHER

%%

"/*"					{ printf("comment start\n");BEGIN(COMMENT);}
<COMMENT>"*/"			{ printf("comment end\n"); BEGIN(OTHER);} 
<COMMENT>([^*]|\n)+		{ /*ignore comment*/ }
<OTHER>[a-zA-Z]*|[0-9]* {ECHO;}
%%

int main(int argc, char **argv) {
	yyin = fopen(argv[1], "r");
	yylex();
	return 0;
}
