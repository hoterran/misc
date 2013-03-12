#include <stdio.h>

int main(int argc, char **argv) {
    FILE *yyin = fopen(argv[1], "r");
    printf("==%p==\n", yyin);
    return 0;
}

