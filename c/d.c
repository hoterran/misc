#include <stdio.h>

int main () {
    int d = 1;
    int *c = &d;

    char z = (char)(*c);

    printf("%c\n", z);

}
