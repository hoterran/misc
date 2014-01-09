#include <iostream>

using namespace std;

typedef int* KKK(char);         // c++
typedef int* (*funcp)(char);    // c

int* k2 (funcp z) {
    return z(1);
}

int* k (KKK z) {
    return z(1);
}

int a = 1;

int* f(char c) {
    return &a;
}

int main()
{
    int *p = k(f);
    int *p2 = k2(f);
    cout << *p << '-' << *p2 << endl;
}

