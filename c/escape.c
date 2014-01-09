#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

char* escape(char *s, size_t len);

int main() {
    char s[] = "abc\n\r\\\'\"";

    char *d = escape(s, sizeof(s));
    printf("[%s]\n[%s]\n", s, d);
}

char* escape(char *s, size_t len) {

    char *c = calloc(1, len*2);
    int i = 0, j = 0;
    for(; i < len ; i++, j++) {
        switch(s[i]) {
            case '\n':
                c[j] = '\\';
                c[j+1] = 'n';
                j++;
                break;
            case '\r':
                c[j] = '\\';
                c[j+1] = 'r';
                j++;
                break;
            case '\'':
                c[j] = '\\';
                c[j+1] = '\'';
                j++;
                break;
            case '\"':
                c[j] = '\\';
                c[j+1] = '"';
                j++;
                break;
            case '\\':
                c[j] = '\\';
                c[j+1] = '\\';
                j++;
                break;
            default:
                c[j] = s[i];
        }
    }
    return c;
}
