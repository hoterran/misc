#include <iostream>

using namespace std;

int main(int argc, char *argv[]) 
{
    string s = string("abc");

    cout << s << endl;

    string::iterator i = s.begin();

    while (i != s.end()) {
        cout << *(i++) << endl; 
    }
}
