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

/*

message SelfDescribingMessage {
    // Set of .proto files which define the type.
    required FileDescriptorSet proto_files = 1;
     
    // Name of the message type.  Must be defined by one of the files in proto_files.
    required string type_name = 2;
      
    // The message data.
    required bytes message_data = 3;
}

*/
    tutorial::SelfDescribingMessage sdmessage;

    fstream desc(argv[2], ios::in | ios::binary);
    sdmessage.mutable_proto_files()->ParseFromIstream(&desc)；
    sdmessage.set_type_name((address_book.GetDescriptor())->full_name());
    sdmessage.clear_message_data();
    address_book.SerializeToString(sdmessage.mutable_message_data());

    fstream output(argv[1], ios::out | ios::trunc | ios::binary);
    sdmessage.SerializeToOstream(&output))；

    return 0;
}
