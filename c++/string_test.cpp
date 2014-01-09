#include <string>
#include <iostream>
#include <stdio.h>

using namespace std;

int main() {
    string s = "abc";

    if (s == "abc") {
        cout <<"ok\n" <<endl;
        char b[10];
        snprintf(b, sizeof(b), "%s", s.c_str());
        cout << string(b) <<endl;
    }
        
    return 0;
}
