#include <stdio.h>
#include <string.h>

/*
0x12 0x33 0x44
*/
int main(int argc, char *argv[]) {
    int i;
    char *c = argv[1];
    char d[3];
    for (i = 0; i < strlen(c); ) {
        if ((c[i] == '0') && (c[i + 1] == 'x')) {
            snprintf(d, sizeof(d), "%s", c + i + 2);
            int i = atoi(d);
            int j = i / 10 * 16 + i % 10;
            printf("%c-", j);
        } else {
            printf("error\n"); 
            return 1;
        }
        i = i + 5;
    }
    printf("\n");
}

