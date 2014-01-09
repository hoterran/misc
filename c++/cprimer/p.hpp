
template <typename T>
void PRINT_ELEMENTS(const T& t, const char *s = "") 
{
    typename T::const_iterator pos;
    std::cout << s;
    for (pos = t.begin(); pos != t.end(); pos++) {
        std::cout << *pos << ' '; 
    }
    std::cout << std::endl;
}
