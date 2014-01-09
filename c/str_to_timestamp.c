#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc ,char *argv[])
{
    if (argc == 1) {
        time_t t = time(NULL);
        printf("%ld\n", t);
    } else {
        // "2013-11-18 14:35:53"
        int year =0, mon = 0, day = 0, hour = 0, min = 0, second = 0;
        sscanf(argv[1], "%d-%d-%d %d:%d:%d", &year, &mon, &day, &hour, &min, &second);
        printf("%d-%d-%d %d:%d:%d\n", year, mon, day, hour, min, second);
        struct tm t;
        t.tm_year = year - 1900;
        t.tm_mon = mon - 1;
        t.tm_mday = day;
        t.tm_hour = hour;
        t.tm_min = min;
        t.tm_sec = second;

        printf("%ld\n", mktime(&t));
    }
	
	return 1;
}
