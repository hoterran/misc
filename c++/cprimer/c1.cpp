#include <string>
#include <iostream>

using namespace std;

class Screen {
    public:
        Screen(int hi=8, int wid=4, char c='1');
        void home() {_cursor = 0;};
        void get (int, int) const;
        char get() const {return _screen[_cursor];} ;
        int i;
        void copy(const Screen &s);
        Screen& display();
        Screen& clear();
        // static function
        static double g() {return intersetRate;};

    private:
        // static variable 
        static double intersetRate;
        string _screen;
        string::size_type _cursor;
        short _height;
        short _width;

};

Screen::Screen(int i, int w, char bk):
    _height(i),
    _width(w),
    _cursor(0),
    _screen( i * w, bk)
{
    this->intersetRate--;
};

void Screen::copy(const Screen &s) {
    if (this != &s) {
       this-> _height = s._height; 
        _width = s._width;
        _screen = s._screen;
    }
}

Screen& Screen::clear() {
    return *this;
}

Screen& Screen::display() {
    return *this;
}

void Screen::get(int a, int b) const {
    
}

double Screen::intersetRate = 10;

int main() {

    Screen s1;
    Screen *s2 = new Screen(1, 2, '2');

    Screen s3;
    s3.home();
    s3.get(1,2);

    s3.copy(*s2);

    cout << "-" << s1.get() << "-" << s2->get() << "-" << s3.get() << "-" << Screen::g() << endl;
    s3.clear().display();
    return 0;
}
