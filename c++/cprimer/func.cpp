#include <iostream>
#include <string>
#include <vector>
#include <iterator>
#include <algorithm>
#include <list>
#include <set>
#include <cstdlib>
#include <functional>
#include <numeric>

using namespace std;
template <typename T>
void printVector(const vector<T> &v)
{
    typename vector<T>::const_iterator it;
    for(it=v.begin();it!=v.end();++it) //使用迭代器输出
        cout<<*it<<" ";
    cout<<endl;
}

double x() {
    return 1;
};

int main()
{
 const int N=10;
 vector<double> v1(N);
 vector<double> v2(N);
 vector<double> v3(N);

 generate(v1.begin(),v1.end(), x);
 fill(v2.begin(),v2.end(), 200); //其值最小为32767,最大为2147483647 
 transform(v1.begin(),v1.end(),v2.begin(),v3.begin(),plus<double>());//v3=v1+v2

 printVector(v1);
 printVector(v2);
 printVector(v3);
    
 transform(v1.begin(),v1.end(),v2.begin(),v3.begin(),minus<double>());//v3=v1+v2

 printVector(v1);
 printVector(v2);
 printVector(v3);
/*
const int M=10;
 vector<double> vec(M);
 for(int j=0;j<M;j++)
 {
  vec[j]=j+1;
 }

 partial_sum(vec.begin(),vec.end(),vec.begin(),multiplies<double>());
 copy(vec.begin(),vec.end(),ostream_iterator<double>(cout," "));
 cout<<endl;
 generate(vec.begin(),vec.end(),x);
 
 transform(vec.begin(),vec.end(),vec.begin(),bind2nd(divides<double>(),double(RAND_MAX)));
 printVector(vec);
 vector<int> a1(M);
 vector<int> a2(M);
 for(j=0;j<M;j++)
 {
  a1[j]=j+1;
 }
 transform(a1.begin(),a1.end(),a1.begin(),negate<int>());
 printVector(a1);
 return 0;
*/
}
