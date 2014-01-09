#define _GNU_SOURCE 
 
#include <stdint.h>
#include <stdio.h>
#include <sched.h>
#include <pthread.h>
#include <stdlib.h>
 
#define app_panic(format, args...) \
    do {    \
        printf(format, ## args);    \
        abort();    \
    } while(0)
 
static inline void print_cpu_mask(cpu_set_t cpu_mask)
{
    uint8_t flag;
    uint32_t i;
    printf("Cpu affinity is ");
    flag = 0;
    for (i = 0; i < sizeof(cpu_set_t); i ++) {
        if (CPU_ISSET(i, &cpu_mask)) {
            if (flag == 0) {
                flag = 1;
                printf("%d", i);
            } else {
                printf(",%d", i);
            }
        }
    }
    printf(".\n");
}
 
static inline void get_cpu_mask(pid_t pid, cpu_set_t *mask) {
    if (sched_getaffinity(pid, sizeof(cpu_set_t), mask) == -1) {
        app_panic("Get cpu affinity failed.\n");
    }
}
 
static inline void set_cpu_mask(pid_t pid, cpu_set_t *mask) {
    if (sched_setaffinity(pid, sizeof(cpu_set_t), mask)
            == -1) {
        app_panic("Set cpu affinity failed.\n");
    }
}
 
int main(int argc, char *argv[])
{
    /* argv[1] is cpu number */
    uint32_t active_cpu = atoi(argv[1]);
    cpu_set_t cpu_mask;
 
    get_cpu_mask(0, &cpu_mask);
    print_cpu_mask(cpu_mask);
 
    CPU_ZERO(&cpu_mask);
    CPU_SET(active_cpu, &cpu_mask);
    set_cpu_mask(0, &cpu_mask);
 
    get_cpu_mask(0, &cpu_mask);
    print_cpu_mask(cpu_mask);
 
    while(1);
    return 0;
}
