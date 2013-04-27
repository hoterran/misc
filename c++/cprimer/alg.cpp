#include <vector>
#include <list>
#include <algorithm>
#include <iostream>

using namespace std;

void printlist(list<int> i) {
    list<int>::iterator iter = i.begin();
    list<int>::iterator iter_end = i.end();

    for (; iter != iter_end; ++iter) {
        cout << *iter << "\n"; 
    }
}

int main() {
    int ia[5] = {1, 2,3,4,5};
    vector <string> svec;
    list <int> dlist;

    vector<string>::iterator viter;
    list<int>::iterator liter;

    dlist.insert(dlist.begin(), ia, ia + 5);

    liter = find(dlist.begin(), dlist.end(), 1);

    dlist.insert(liter, 100);
    printlist(dlist);

}
