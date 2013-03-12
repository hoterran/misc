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
%left '-' '+'
%left '*' '/'
%left NEG
%right '^'

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
	| exp '+' exp	{ $$ = $1 + $3; }
	| exp '-' exp   { $$ = $1 - $3; }
	| exp '*' exp   { $$ = $1 * $3; }
	| exp '/' exp   { $$ = $1 / $3; }
	| exp '^' exp   { $$ = pow($1, $3);}
	| '-' exp %prec NEG		{ $$ = -$2;}
	| '(' exp ')'	{ $$ = $2;}
%%

#include <ctype.h>

int yylex(void) {
	int c;
	while((c = getchar()) == ' ' || (c == '\t'))
		continue;
	if (c == '.' || isdigit(c)) {
		ungetc(c, stdin);
		scanf("%lf", &yylval); // c -> yylval
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
	return yyparse();
}
