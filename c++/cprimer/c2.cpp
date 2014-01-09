#include <iostream>

class Data {
    public:
        int val;
        char *ptr;
};

int main() {
    Data d1(1111, "aaa");
    Data d2(222);
    Data d3;

    cout << d1.val << d2.val << d3.val  << endl;
}
