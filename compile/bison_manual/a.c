#include <stdio.h>
#include <unistd.h>

int main() {
    int c;
    while((c = getchar()) == ' ' || (c == '\t'))
        continue;
    if (c == '.' || isdigit(c)) {
        ungetc(c, stdin);
        scanf("%lf", &yylval);
        printf("---%lf---", yylval);
        return NUM;
    }
    if (c == EOF)
        return 0;
    return c;

}
