#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

static int x;

#define N 100
#define M 10000

void loop () {
    int j = 0;
    for (j=0;j<M;j++) {
        x++;
    }
}

void atom_loop () {
    int j = 0;
    for (j=0;j<M;j++) {
        __sync_fetch_and_add(&x, 1);
    }
}


int main(int argc, char *argv[])
{
    int i;
    x = 0;
    pthread_t p[N] = {};
    for (i = 0; i< N; i++) {
        pthread_create(&p[i], NULL, (void *)atom_loop, NULL);
    }
    for (i = 0; i< N ; i++) {
        pthread_join(p[i], NULL);
    }
    printf("%d-%d\n", M * N , x);
}

