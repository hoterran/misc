#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/time.h>

/*
 * gcc -g -o d set-timer.c
 */

typedef int timer_id;

typedef int timer_expiry(timer_id id, void *user_data, int len);

struct timer {
    LIST_ENTRY(timer) entires; /* list */
    timer_id id;

    int interval;
    int elapse;

    timer_expiry *cb;           /* fuc */
    void *user_data;             /* arg */
    int len;
}

struct timer_list {
    LIST_HEAD(listheader, timer) header; /* header */
    int num;
    int max_num;

    void (*old_sigfunc)(int);
    void (*new_sigfunc)(int);

    struct itimerval ovalue;
    struct itimerval value;
};

struct timer_list tl;

static void sig_func(int signo) {

    struct timer *node = timer_list.header.lh_first;
    for (; node != NULL; node = node->entries.le_next) {
        node->elapse++;
        if (node->elapse >= node->interval) {
            node->elapse = 0;
            node->cb(node->id, node->user_data, node->len);
        }
    }
}

int init_timer(int count) {
    int ret = 0;
    
    LIST_INIT(&tl.header);
    tl.max_num = count;

    /* save old signal header */
    tl.old_sigfunc = signal(SIGALRM, sig_func);
    tl.new_sigfunc = sig_func;

    tl.value.it_value.tv_sec = TIMER_START;
    tl.value.it_value.tv_usec = 0;
    tl.value.it_interval.tv_sec = TIMER_TICK;
    tl.value.it_interval.tv_usec = 0;

    ret = setitimer(ITIMER_REAL, &timer_list.value, NULL);

    return ret;
}

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


