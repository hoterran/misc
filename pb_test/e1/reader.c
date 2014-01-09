#include <iostream>
#include <fstream>
#include "addressbook.pb.h"

using namespace std;

void ListMsg(const lm::helloworld &msg) 
{
    cout << msg.id() <<endl;
    cout << msg.str() <<endl;
    cout << msg.opt() <<endl;
}

int main(int argc, char *argv[])
{
    lm::helloworld msg1;

    {
        fstream input("./log", ios::in | ios::binary);
        if (!msg1.ParseFromIstream(&input)) {
            cerr <<"Fail" <<endl;
            return -1;
        }
    
    }

    ListMsg(msg1);
}
