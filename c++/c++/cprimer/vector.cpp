#include <vector>
#include <iostream>
#include <list>

using namespace std;

void printlist(list<string> i) {
    list<string>::iterator iter = i.begin();
    list<string>::iterator iter_end = i.end();

    for (; iter != iter_end; ++iter) {
        cout << *iter << "\n"; 
    }
}

void print(vector<string> ivec) {
    vector<string>::iterator iter = ivec.begin();
    vector<string>::iterator iter_end = ivec.end();

    for (; iter != iter_end; ++iter) {
        cout << *iter << "\n"; 
    }
}

int main()
{
    //---- vector
    vector< int> ivec;
    cout << ivec.size() << "-" << ivec.capacity() << endl;

    for (int i = 0; i < 24; ++i) {
        ivec.push_back(i); 
        cout << ivec.size() << "-" << ivec.capacity() << endl;
    }

    vector <string> ivec2;
    ivec2.reserve(32); //set capacity
    cout << ivec2.size() << "-" << ivec2.capacity() << endl;

    for (int i = 0; i < 24; ++i) {
        ivec2.push_back("xx"); 
        cout << ivec2.size() << "-" << ivec2.capacity() << endl;
    }
    print(ivec2);

    // copy 
    vector<string> ivec3(ivec2.begin(), ivec2.end());
    print(ivec3);

    // list
    vector <string> svec;
    svec.insert(svec.begin(), "abc");
    svec.insert(svec.begin(), "def");
    print(svec);

    list <string> slist;
    slist.insert(slist.begin(), "cde");
    slist.insert(slist.begin(), "oiu");
    slist.insert(slist.end(), "mnh");
    printlist(slist);
    slist.pop_back();
    slist.pop_front();
    cout <<"--" << "\n";
    printlist(slist);
    cout <<"--" << "\n";

    vector<string> vec3;
    string ana("Ana");
    vec3.insert(vec3.begin(), 3, ana);
    vec3.erase(vec3.begin());
    vec3.pop_back();
    //vec3.pop_front();
    string s[4] = {"abc", "def", "jjh", "oii"};
    vec3.insert(vec3.end(), s, s + 4);   

    print(vec3);

    // copy

    vector<string> vec4 = vec3; //deep copy
    vec3.pop_back();
    cout << vec4.size() << endl;
}
