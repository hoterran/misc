#include <vector>
#include <iostream>
#include <list>
#include "p.hpp"

using namespace std;

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
    PRINT_ELEMENTS(ivec2);

    // copy 
    vector<string> ivec3(ivec2.begin(), ivec2.end());
    PRINT_ELEMENTS(ivec3);

    // list
    vector <string> svec;
    svec.insert(svec.begin(), "abc");
    svec.insert(svec.begin(), "def");
    PRINT_ELEMENTS(svec);

    list <string> slist;
    slist.insert(slist.begin(), "cde");
    slist.insert(slist.begin(), "oiu");
    slist.insert(slist.end(), "mnh");
    PRINT_ELEMENTS(slist);
    slist.pop_back();
    slist.pop_front();
    cout <<"--" << "\n";
    PRINT_ELEMENTS(slist);
    cout <<"--" << "\n";

    vector<string> vec3;
    string ana("Ana");
    vec3.insert(vec3.begin(), 3, ana);
    vec3.erase(vec3.begin());
    vec3.pop_back();
    //vec3.pop_front();
    string s[4] = {"abc", "def", "jjh", "oii"};
    vec3.insert(vec3.end(), s, s + 4);   

    PRINT_ELEMENTS(vec3);

    // copy

    vector<string> vec4 = vec3; //deep copy
    vec3.pop_back();
    cout << vec4.size() << endl;

    // size_type
    typedef vector<string>::size_type vec_sz;
    vec_sz m = svec.size();
    svec.push_back("a");
    cout << "-" << m << "-" << svec.size() << endl;

    // copy
    vector<string> v2;
    v2.resize(svec.size()); // must , avoid segmentfault
    copy(svec.begin(), svec.end(), v2.begin());
    PRINT_ELEMENTS(v2, "copy "ck_inserter);

    
}
