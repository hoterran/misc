#include <map>
#include <iostream>
#include <vector>

using namespace std;

void print(map<string,int> m) {
    map<string,int>::iterator iter = m.begin();

    for (; iter != m.end(); ++iter) {
        cout << iter->first << "-" << iter->second << "\n"; 
    }
}

void printv(vector<int> ivec) {
    vector<int>::iterator iter = ivec.begin();
    vector<int>::iterator iter_end = ivec.end();

    for (; iter != iter_end; ++iter) {
        cout << *iter << "\n"; 
    }
}

void printmap(map<string,vector<int> > m) {
    map<string,vector<int> >::iterator iter = m.begin();

    for (; iter != m.end(); ++iter) {
        cout << iter->first << "-" <<endl;
        printv(iter->second);
    }
}

int main() 
{
    map<string, int> word_count;
   
    word_count["aaaa"] = 100;
    word_count["aaab"] = 2;
    word_count["aaac"] = 3;

    word_count.insert(map<string,int>::value_type(string("kkk"), 1));
    print(word_count);

    if (1 == word_count.count("aaaa"))
        cout << word_count["aaaa"] <<endl;

    map<string,int>::iterator it = word_count.find("aaab");
    if (it != word_count.end())
        cout << it->second << endl;

    // 

    typedef vector<int> ve;
    map<string, ve> phone;
    typedef map<string,ve>::value_type vt;


    ve v1, v2;
    v1.insert(v1.begin(), 10, 1);
    v2.insert(v2.begin(), 10, 2);
    phone.insert(vt(string("rry"), v1)); 
    phone.insert(vt(string("ll"), v2));  

    printmap(phone);

    phone.erase("rry");
    printmap(phone);
    map<string,ve>::iterator it2 = phone.find("ll");
    if (it2 != phone.end())
        cout << it2->first << endl;
}
