#include <iostream>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
using namespace std;

static const char UTF8_BYTE_PREFIX = 0x80;
static const char UTF8_BYTE_MASK = 0xc0;

bool _ValidateEncodingUTF8(string& str)
{
    string::iterator iter = str.begin();
    while(iter != str.end())
    {
        uint16_t unicode = 0;
        char c;
        if((*iter & 0x80) == 0x00) // one byte
        {
            /**
             * mapping rule: 0000 0000 - 0000 007F | 0xxxxxxx
             */
            //nothing to check
            iter++;
        }
        else if((*iter & 0xe0) == 0xc0) // two bytes
        {
            /**
             * mapping rule: 0000 0080 - 0000 07FF | 110xxxxx 10xxxxxx
             */
            c = *iter;
            if (iter + 1 == str.end()) {
                // only 110xxxxxx
                *iter = '_';
                return false;
            }
            /* check whether the byte is in format 10xxxxxx */
            if ((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                iter = iter + 2;
                continue;
            }
            /* get unicode value */
            unicode = (((c & 0x1f) << 6) | (*(iter + 1) & 0x3f));
            /* validate unicode range */
            if (!(unicode >= 0x80 && unicode <= 0x7ff)) {
                *iter = '_';
                *(iter + 1) = '_';
                iter = iter + 2;
                continue;
            }
            iter = iter + 2;
        }
        else if((*iter & 0xf0) == 0xe0) // three bytes
        {
            /**
             * mapping rule: 0000 0800 - 0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
             */
            c = *iter;
            if (iter + 1 == str.end()) {
                // only 110xxxxxx
                *iter = '_'; 
                return false;
            }
            if (iter + 2 == str.end()) {
                // only 110xxxxxx
                *(iter + 1) = '_'; 
                return false;
            }
            /* check whether the byte is in format 10xxxxxx */
            if ((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                iter = iter + 2;
                continue;
            }
            if ((*(iter + 2) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                *(iter + 2) = '_';
                iter = iter + 3;
                continue;
            }
            /* get unicode value */
            unicode = (((c & 0x0f) << 12) | ((*(iter + 1) & 0x3f) << 6) | (*(iter + 2) & 0x3f));
            /* validate unicode range */
            if (!(unicode >= 0x800 /* && unicode <= 0xffff */)) {
                *iter = '_';
                *(iter + 1) = '_';
                *(iter + 2) = '_';
                iter = iter + 3;
                continue;
            }
            iter = iter + 3;
        }
        else if((*iter & 0xf8) == 0xf0) // four bytes
        {
            /**
             * mapping rule: 0001 0000 - 0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             */
            c = *iter;
            if (iter + 1 == str.end()) {
                // only 110xxxxxx
                *iter = '_'; 
                return false;
            }
            if (iter + 2 == str.end()) {
                // only 110xxxxxx
                *(iter + 1) = '_'; 
                return false;
            }
            if (iter + 3 == str.end()) {
                // only 110xxxxxx
                *(iter + 2) = '_'; 
                return false;
            }
            /* check whether the byte is in format 10xxxxxx */
            if ((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                iter = iter + 2;
                continue;
            }
            if ((*(iter + 2) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                *(iter + 2) = '_';
                iter = iter + 3;
                continue;
            }
            if ((*(iter + 3) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                *iter = '_';
                *(iter + 1) = '_';
                *(iter + 2) = '_';
                *(iter + 3) = '_';
                iter = iter + 4;
                continue;
            }
            /* get unicode value */
            uint32_t unicode = 0x00000000; // for 4 bytes utf-8 encoding
            unicode = ((c & 0x07) << 18) | ((*(iter + 1) & 0x3f) << 12) | ((*(iter + 2) & 0x3f) << 6) | ((*(iter + 3)& 0x3f));
            if (!(unicode >= 0x00010000 && unicode <= 0x0010ffff)) { /* validate unicode range */
                *iter = '_';
                *(iter + 1) = '_';
                *(iter + 2) = '_';
                *(iter + 3) = '_';
                iter = iter + 4;
                continue;
            }
            iter = iter + 4;
        }
        else
        {
            *iter = '_';
            iter++;
            continue;
        }
    }
    return true;
}

bool ValidateEncodingUTF8(const string& str)
{
#define FAIL_IF_TRUE(stat) if(stat) return false;
    string::const_iterator iter = str.begin();
    while(iter != str.end())
    {
        uint16_t unicode = 0;
        char c;
        if((*iter & 0x80) == 0x00)// one byte
        {
            /**
             * mapping rule: 0000 0000 - 0000 007F | 0xxxxxxx
             */
            //nothing to check
        }
        else if((*iter & 0xe0) == 0xc0) // two bytes
        {
            /**
             * mapping rule: 0000 0080 - 0000 07FF | 110xxxxx 10xxxxxx
             */
            c = *iter;
            FAIL_IF_TRUE(iter + 1 == str.end());
            /* check whether the byte is in format 10xxxxxx */
            FAIL_IF_TRUE((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            /* get unicode value */
            unicode = (((c & 0x1f) << 6) | (*++iter & 0x3f));
            /* validate unicode range */
            FAIL_IF_TRUE(!(unicode >= 0x80 && unicode <= 0x7ff));
        }
        else if((*iter & 0xf0) == 0xe0) // three bytes
        {
            /**
             * mapping rule: 0000 0800 - 0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
             */
            c = *iter;
            FAIL_IF_TRUE((iter + 1 == str.end()) || (iter + 2 == str.end()));
            /* check whether the byte is in format 10xxxxxx */
            FAIL_IF_TRUE((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            FAIL_IF_TRUE((*(iter + 2) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            /* get unicode value */
            unicode = (((c & 0x0f) << 12) | ((*++iter & 0x3f) << 6) | (*++iter & 0x3f));
            /* validate unicode range */
            FAIL_IF_TRUE(!(unicode >= 0x800 /* && unicode <= 0xffff */));
        }
        else if((*iter & 0xf8) == 0xf0) // four bytes
        {
            /**
             * mapping rule: 0001 0000 - 0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             */
            c = *iter;
            FAIL_IF_TRUE((iter + 1 == str.end()) || (iter + 2 == str.end()) || (iter + 3 == str.end()));
            /* check whether the byte is in format 10xxxxxx */
            FAIL_IF_TRUE((*(iter + 1) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            FAIL_IF_TRUE((*(iter + 2) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            FAIL_IF_TRUE((*(iter + 3) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX);
            /* get unicode value */
            uint32_t unicode = 0x00000000; // for 4 bytes utf-8 encoding
            unicode = ((c & 0x07) << 18) | ((*++iter & 0x3f) << 12) | ((*++iter & 0x3f) << 6) | ((*++iter & 0x3f));
            /* validate unicode range */
            FAIL_IF_TRUE(!(unicode >= 0x00010000 && unicode <= 0x0010ffff));
        }
        else
        {
            return false;
        }
        ++iter;
    }
    return true;
#undef FAIL_IF_TRUE
}

int main(int argc, char *argv[]) 
{
    FILE *f = fopen(argv[1], "r");
    char s[3000];

    while (NULL != fgets(s, (int)sizeof(s), f)) {
        string z = string(s);
        _ValidateEncodingUTF8(z);
        cout << z << endl;
    }
}
