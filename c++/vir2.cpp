#include <iostream>

using namespace std;

class A {
    public:
        virtual void f() const { cout << "A::f" << endl; }
        virtual void g(int) const { cout << "A::g" << endl; }
};

class B: public A {
    public:
        void f() const { cout << "B::f" << endl; }
        void g(int) const { cout << "B::g" << endl; }
        void g(char) const { cout << "B::g2" << endl; }
};

void call(const A& a) {
    a.f();
    a.g(1);
    a.g('1');
}

int main() {
    A a;
    B b;
    call(a);
    call(b);
}
