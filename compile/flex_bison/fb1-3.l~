%%

"+"	{ printf("PLUS\n"); }
"-"	{ printf("MINUS\n"); }
"*"	{ printf("TIMES\n");  }
"/"	{ printf("DIVIDE\n"); }
[0-9]+  { printf("NUMBER %s\n", yytext); }
\n	{ printf("NEWLINE\n"); }
[ \t]	{ }
.	{ printf("Mystery %s\n", yytext); }

%%
