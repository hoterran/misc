#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void binary(char c) {
    int i;
    for (i = 7; i >= 0; i--) {
        if(c & (1 << i))
            printf("1");
        else
            printf("0");
    }
}

int main(int argc, char* argv[]) {
    FILE *f = fopen(argv[1], "r");
    char s[1000] = {};
    fread(s, 1, 10000, f);
    int l = strlen(s);
    int i;
    printf("--------------------%d----------------------", l);
    for (i = 0; i < l; i++) {
        binary(s[i]);
        printf(" ");
    }
    printf("\n");
    for (i = 0; i < l; i++) {
        printf("%8d", s[i]);
        printf(" ");
    }
    printf("\n");
    return 0;
}
