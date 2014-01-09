#include <iostream>

using namespace std;

class A {
    private:
        int _a;
        int _b;
    public:
        void set(int a, int b) {_a = a; _b = b;};
        int get_a() const { return _a;};
        int get_b() const { return _b;};
        void operator=(const A& a);
};

void A::operator= (const A& a) {
    _a = a.get_a();
    _b  = a.get_b();
};

ostream& operator<<(ostream& os, const A& a) {
    return os << a.get_a() << '-' << a.get_b(); 
};

int main() {
    A a;
    a.set(1,2);
    A b;
    b = a;
    cout << a << b << endl;
}
