#include <iostream>

using namespace std;

class A {
    public:
        virtual void f() const { cout << "A::f" << endl; }
        void g() const { cout << "A::g" << endl; }
};

class B: public A {
    public:
        void f() const { cout << "B::f" << endl; }
        void g() const { cout << "B::g" << endl; }
};

class C: public B {
    public:
        void f() { cout << "C::f" << endl; } // miss const 
        void g() const { cout << "C::g" << endl; }
};

void call(const A& a) {
    a.f();
    a.g();
}

int main() {
    A a;
    B b;
    C c;
    call(a);
    call(b);
    call(c);
}
