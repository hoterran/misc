#include <stdio.h>
#include <pthread.h>

int flag = 0;

void* func(void*arg) {
    long i = 0;
    while(!flag) {
        i++;
    }
    printf("done %ld\n", i);
}

int main() {

    pthread_t t;
    int *status;

    pthread_create(&t, NULL, func, NULL);

    sleep(2000);
    flag = 1;
    printf(" flag done\n");

    pthread_join(t, &status);
}
