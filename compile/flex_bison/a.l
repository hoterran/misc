%{
#include <stdio.h>
%}

%%
[0-9]+		printf("NUMBER\n");
[a-zA-Z]+	REJECT; printf("WORD\n");
%%

