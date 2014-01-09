#include <iostream>
#include <functional>
#include <algorithm>
#include <vector>
using namespace std;

int main()
{
    vector<int> coll;
    for(int i = 1; i <= 10; ++i)
    {
        coll.push_back(i);
    }
    //查找元素值大于10的元素的个数
    //也就是使得 10 less elem 成立的元素个数
    // (const Operation& op, const T& x)
    // x op value 
    int res = count_if(coll.begin(), coll.end(), bind1st(less<int>(), 10));
    cout << res << endl;
    //查找元素值小于10的元素的个数
    //也就是使得 elem less 10 成立的元素个数
    // (const Operation& op, const T& x)
    // value op x
    res = count_if(coll.begin(), coll.end(), bind2nd(less<int>(), 10));
    cout << res << endl;
    return 0;
}
