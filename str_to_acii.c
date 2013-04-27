#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
    int i;
    for (i = 0; i <strlen(argv[1]); i++) {
        printf("%d ", argv[1][i]); 
    }
    printf("\n");
    for (i = 0; i <strlen(argv[1]); i++) {
        printf("0x%.2x ", argv[1][i]); 
    }
    printf("\n");
    return 0;
}

