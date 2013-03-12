%option noyywrap

%{
	int chars = 0;
	int words = 0;
	int lines = 0;
%}

%%
[a-zA-Z]+	{words++; chars += strlen(yytext);}
\n			{lines++;}
.			{chars++;}
%%

main(int argc, char **argv) {

	int i;
	for (i = 1; i < argc; i++) {

		FILE *f = fopen(argv[i], "r");
		if (!f) continue;
		yyrestart(f);
		yylex();
		fclose(f);
		printf("%8d%8d%8d\n", lines, words, chars);
		chars = 0;
		words = 0;
		lines = 0;
	}

	return 0;
}

