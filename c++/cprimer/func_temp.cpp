#include <iostream>

using namespace std;

template <class Type>
Type mmin(Type a, Type b) {
    return a < b ? a:b;
}

int main() 
{
    cout << mmin(10, 20) << endl;
}
