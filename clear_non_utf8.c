#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

static const char UTF8_BYTE_PREFIX = 0x80;
static const char UTF8_BYTE_MASK = 0xc0;

int clear_non_ascii_or_utf8(char *str, size_t len)
{
    unsigned int i = 0;
    while(i < len) {
        uint16_t unicode = 0;
        if((str[i] & 0x80) == 0x00) // one byte
        {
            i++;
        }
        else if((str[i] & 0xe0) == 0xc0) // two bytes
        {
            if (i + 1 > len) {
                str[i] = '_';
                return 1;
            }
            /* check whether the byte is in format 10xxxxxx */
            if (((str[i + 1]) & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_';
                i = i + 2;
                continue;
            }
            /* get unicode value */
            unicode = (((str[i] & 0x1f) << 6) | (str[i + 1] & 0x3f));
            /* validate unicode range */
            if (!(unicode >= 0x80 && unicode <= 0x7ff)) {
                str[i] = '_';
                str[i + 1] = '_';
                i = i + 2;
                continue;
            }
            i = i + 2;
        }
        else if((str[i] & 0xf0) == 0xe0) // three bytes
        {
            if (i + 1 > len) {
                str[i] = '_'; 
                return 1;
            }
            if (i + 2 > len) {
                str[i] = '_'; 
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_'; 
                return 1;
            }
            /* check whether the byte is in format 10xxxxxx */
            if ((str[i + 1] & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_';
                i = i + 2;
                continue;
            }
            if ((str[i + 2] & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
                str[i + 1] = '_';
				if ((str[i + 2] & 0x80) != 0x00)
					str[i + 2] = '_';
                i = i + 3;
                continue;
            }
            /* get unicode value */
            unicode = (((str[i] & 0x0f) << 12) | (( str[i + 1] & 0x3f) << 6) | (str[i + 2] & 0x3f));
            /* validate unicode range */
            if (!(unicode >= 0x800 /* && unicode <= 0xffff */)) {
                str[i] = '_';
                str[i + 1] = '_';
                str[i + 2] = '_';
                i = i + 3;
                continue;
            }
            i = i + 3;
        }
        else if((str[i] & 0xf8) == 0xf0) // four bytes
        {
            if (i + 1 > len) {
                str[i] = '_'; 
                return 1;
            }
            if (i + 2 > len) {
                str[i] = '_';
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_';
                return 1;
            }
            if (i + 3 > len) {
                str[i] = '_';
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_';
				if ((str[i + 2] & 0x80) != 0x00)
					str[i + 2] = '_';
                return 1;
            }
            /* check whether the byte is in format 10xxxxxx */
            if ((str[i + 1] & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
				if ((str[i + 1] & 0x80) != 0x00)
					str[i + 1] = '_';
                i = i + 2;
                continue;
            }
            if ((str[i + 2] & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
                str[i + 1] = '_';
				if ((str[i + 2] & 0x80) != 0x00)
					str[i + 2] = '_';
                i = i + 3;
                continue;
            }
            if ((str[i + 3] & UTF8_BYTE_MASK) != UTF8_BYTE_PREFIX) {
                str[i] = '_';
                str[i + 1] = '_';
                str[i + 2] = '_';
				if ((str[i + 3] & 0x80) != 0x00)
					str[i + 3] = '_';
                i = i + 4;
                continue;
            }
            uint32_t unicode = 0x00000000; // for 4 bytes utf-8 encoding
            unicode = ((str[i] & 0x07) << 18) | ((str[i + 1] & 0x3f) << 12) |
                ((str[i + 2] & 0x3f) << 6) | ((str[i + 3]& 0x3f));
            if (!(unicode >= 0x00010000 && unicode <= 0x0010ffff)) { /* validate unicode range */
                str[i] = '_';
                str[i + 1] = '_';
                str[i + 2] = '_';
                str[i + 3] = '_';
                i = i + 4;
                continue;
            }
            i = i + 4;
        }
        else
        {
            str[i] = '_';
            i++;
            continue;
        }
    }
    return 0;
}

int main(int argc, char *argv[]) 
{
    FILE *f = fopen(argv[1], "r");
    char s[3000];

    while (NULL != fgets(s, (int)sizeof(s), f)) {
        clear_non_ascii_or_utf8(s, strlen(s));
        printf("%s\n", s);
    }
}
