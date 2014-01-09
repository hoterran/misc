#include <stdio.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char *argv[]) {

    time_t t;
    t = 1;

    int i = 0;
    int c = atoi(argv[1]);
    struct tm* tm1;

    for (i = 0; i < c; i++) {
        tm1 = localtime(&t);
    }
}
