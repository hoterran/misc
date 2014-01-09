#include <stdio.h>
#include <time.h>

int main() {
    time_t t1 = time(NULL);
    printf("time %ld\n", t1);

    printf("ctime %s\n", ctime(&t1));

    struct tm *t = gmtime(&t1);

    printf("asctime %s\n", asctime(t));
    printf("gmtime %d-%d-%d %d:%d:%d\n", t->tm_year, t->tm_mon, t->tm_mday, t->tm_hour, t->tm_min, t->tm_sec);

    t = localtime(&t1);
    printf("localtime %d-%d-%d %d:%d:%d\n", t->tm_year, t->tm_mon, t->tm_mday, t->tm_hour, t->tm_min, t->tm_sec);

    printf("asctime %s\n", asctime(t));

    printf("mktime %ld\n", mktime(t));
}
