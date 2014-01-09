#include <iostream>
#include <fstream>
#include "addressbook.pb.h"

using namespace std;

int main(int argc, char* argv[])
{
    tutorial::AddressBook address_book;
    tutorial::Person* person1 = address_book.add_person();

    person1->set_id(100);
    person1->set_email("ok@14.com");

    tutorial::Person_PhoneNumber* phone1 = person1->add_phone();
    tutorial::Person_PhoneNumber* phone2 = person1->add_phone();

    phone1->set_number("12345");
    phone1->set_type(tutorial::Person_PhoneType_MOBILE);

    phone2->set_number("7654");
    phone2->set_type(tutorial::Person_PhoneType_HOME);

    tutorial::Person* person2 = address_book.add_person();

    person2->set_id(101);
    person2->set_email("14@com");


    {
        fstream output("./log", ios::out | ios::trunc | ios::binary);
        
        address_book.SerializeToOstream(&output);
    }

    return 0;
}
