#include <iostream>
#include <string>

using namespace std;

void foo1(int x) {
    x = 2;
}

void foo2(int &x) {
    x = 2;
}

void foo3(string s) {
    s = string("efg");
}

void foo4(string &s) {
    s = string("efg");
}

int main() {
    int x = 1;
    foo1(x);
    cout << x << endl; 
    foo2(x);
    cout << x << endl; 

    string s  = string("abc");

    foo3(s);
    cout << s << endl; 
    foo4(s);
    cout << s << endl; 

    return 0;
}

