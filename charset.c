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
    char *s = argv[1];
    int l = strlen(s);
    int i;

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
