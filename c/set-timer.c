#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/time.h>

/*
 * gcc -g -o d set-timer.c
 */
void printMsg(int num) {
    printf("hello world\n");
}

int main() {
    int res = 0;
    signal(SIGALRM, printMsg);
    
    struct itimerval tick;
    // first time
    tick.it_value.tv_sec = 1;
    tick.it_value.tv_usec = 0;
    // interval time
    tick.it_interval.tv_sec = 1;
    tick.it_interval.tv_usec = 0;

    // timer will send SIGALRM signal
    res = setitimer(ITIMER_REAL, &tick, NULL);

    while(1) {
        pause(); 
    }
    return 0;
}


