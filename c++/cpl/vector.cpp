#include <iostream>

struct Entry {
    string name;
    int number;
};

Entry phone_book[1000];

void print_entry(int i) {
    cout << phone_book[i].name << " " << phone_book[i].number << "\n";
}

int main() {
}
