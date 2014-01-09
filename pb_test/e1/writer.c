#include <iostream>
#include <fstream>
#include <string>
#include "addressbook.pb.h"

using namespace std;

int main(void) 
{
    lm::helloworld msg1;
    msg1.set_id(101);
    msg1.set_str("hello, haha");
    msg1.set_opt(1000);

    fstream output("./log", ios::out | ios::trunc | ios::binary);

    if (!msg1.SerializeToOstream(&output)) {
        cerr<<"Fail" <<endl;
        return -1;
    }
    return 0;
}
