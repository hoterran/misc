#include <stdio.h>

int main() {
    unsigned int i = 10000;
    unsigned int i2 = 40000;
    unsigned int i3 = 70000;
    unsigned int i4 = 10000000;

    char s[20];

    snprintf(s, 20, "%hd", i);
    printf("%s\n", s);

    // bad
    snprintf(s, 20, "%hd", i2);
    printf("%s\n", s);

    // bad
    snprintf(s, 20, "%hu", i3);
    printf("%s\n", s);

    snprintf(s, 20, "%hu", i4);
    printf("%s\n", s);

}
