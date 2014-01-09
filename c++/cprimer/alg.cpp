#include <vector>
#include <list>
#include <deque>
#include <set>
#include <algorithm>
#include <iostream>
#include "p.hpp"

using namespace std;

bool compare(int a, int b) {
    return !(b > a); //revert
}

int main() {
    int ia[5] = {1, 2,10,4,5};
    vector <int> svec;
    list <int> dlist;

    vector<string>::iterator viter;
    list<int>::iterator liter;

    dlist.insert(dlist.begin(), ia, ia + 5);

    liter = find(dlist.begin(), dlist.end(), 10);

    dlist.insert(liter, 100);
    PRINT_ELEMENTS(dlist);
    dlist.sort();
    PRINT_ELEMENTS(dlist);

    svec.insert(svec.begin(), ia, ia+5);
    PRINT_ELEMENTS(svec);
    sort(svec.begin(), svec.end());
    PRINT_ELEMENTS(svec);
    sort(svec.begin(), svec.end(), compare);
    PRINT_ELEMENTS(svec);

    cout << *min_element(svec.begin(), svec.end()) << endl;
    cout << *max_element(svec.begin(), svec.end()) << endl;

    vector<int>::iterator iter = find(svec.begin(), svec.end(), 10);
    PRINT_ELEMENTS(svec, "before ");
    reverse(iter, svec.end());
    PRINT_ELEMENTS(svec, "reverse ");

    //copy

    vector<int> d;
    copy(svec.begin(), svec.end(), back_inserter(d));
    PRINT_ELEMENTS(svec, "copy back");

    deque<int> d2;
    copy(svec.begin(), svec.end(), front_inserter(d2));
    PRINT_ELEMENTS(svec, "copy front");

    set<int> d3;
    copy(svec.begin(), svec.end(), inserter(d3, d3.begin()));
    PRINT_ELEMENTS(svec, "copy inserter");

    //find
    vector<int>::iterator pos1, pos2;
    pos1 = find(d.begin(), d.end(), 2);
    pos2 = find(d.begin(), d.end(), 4);

    cout << "max: " << *max_element(pos1, pos2) << endl;
}
