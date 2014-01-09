#include <stdio.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char *argv[]) {

    time_t t;
    t = 1;

    int i = 0;
    int c = atoi(argv[1]);
    printf("%d\n", c);
    struct tm* tm1;

    for (i = 0; i < c; i++) {
        tm1 = localtime(&t);
    }

    printf("%d-%d-%d-%d-%d-%d\n", tm1->tm_year, tm1->tm_mon, tm1->tm_mday,
        tm1->tm_hour, tm1->tm_min, tm1->tm_sec);

    /*
    sleep(3);
    tm1 = gmtime(&t);

    printf("%d-%d-%d-%d-%d-%d\n", tm1->tm_year, tm1->tm_mon, tm1->tm_mday,
        tm1->tm_hour, tm1->tm_min, tm1->tm_sec);
    */
}
