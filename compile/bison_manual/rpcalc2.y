%{
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#define YYSTYPE double
#include <math.h>
int yylex(void);
void yyerror(char *);
%}

%token NUM

%%
input:
	| input line
	;

line:
	'\n'
	| exp '\n' { printf("%.10g\n", $1);}
	;

exp:
	NUM				{ $$ = $1; }
	| exp exp '+'	{ $$ = $1 + $2; }
	| exp exp '-'   { $$ = $1 - $2; }
	| exp exp '*'   { $$ = $1 * $2; }
	| exp exp '/'   { $$ = $1 / $2; }
	| exp exp '^'   { $$ = pow($1, $2);}
	| exp 'n'		{ $$ = -$1;}
%%

#include <ctype.h>

FILE *yyin;

int yylex(void) {
	int c;
	while((c = getc(yyin)) == ' ' || (c == '\t'))
		continue;
	if (c == '.' || isdigit(c)) {
		ungetc(c, yyin);
		fscanf(yyin, "%lf", &yylval); // c -> yylval
		printf("---%lf---", yylval);
		return NUM;
	}
	if (c == EOF)
		return 0;
	return c;
}

#include <stdio.h>

void yyerror(char *s) {
	fprintf(stderr, "%s\n", s);
}

int main() {
	yyin = fopen("a.txt", "r");
	return yyparse();
}
