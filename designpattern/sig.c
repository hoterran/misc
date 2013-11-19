#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

typedef struct _Data {
    void *data;
} Data;

void* get() {

    static Data *d = NULL;

    if (NULL == d)
        d = malloc(sizeof(*d));

    return (void*)d;
}

int main() {

    Data *d = get();
    Data *d2 = get();

    printf("%p=%p\n", d, d2);
}
