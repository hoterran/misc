#include <iostream>
#include <vector>

using namespace std;

void f(int a, int *b, int &c) {
    a++;
    (*b)++;
    c++;
};

void f2(string a, string *b, string &c) {
    a = "2";
    *b = "2";
    c = "2";
}

void f3(vector<int> a, vector<int> *b, vector<int> &c) {
    a.push_back(2);
    (*b).push_back(2);
    c.push_back(2);
}

int main() {
    int a = 1, b = 1, c = 1;
    f(a, &b, c);
    cout<<a<<b<<c<<endl;    //1 2 2 

    string a1(1, '1');
    string b1(1, '1');
    string c1(1, '1');
    
    f2(a1, &b1, c1);
    cout<<a1<<b1<<c1<<endl; //1 2 2 

    vector<int> v1, v2, v3;
    v1.push_back(1);
    v2.push_back(1);
    v3.push_back(1);
    f3(v1, &v2, v3);
    cout<<v1.size()<<v2.size()<<v3.size()<<endl; //  1 2 2
}
