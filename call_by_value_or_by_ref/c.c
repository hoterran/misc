#include <stdio.h>

void foo1(int x) {
    x = 2;
    printf("address(%p), value(%d)\n", &x, x);
}

void foo3(int *x) {
    *x = 3;
    printf("address(%p), value(%p), pointer(%d)\n", &x, x, *x);
}

int main() {
    int x = 1;
    printf("address(%p), value(%d)\n", &x, x);

    foo1(x);
    printf("address(%p), value(%d)\n", &x, x);

    foo3(&x);
    printf("address(%p), value(%d)\n", &x, x);

}
