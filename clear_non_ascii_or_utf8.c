#include <string.h>
#include <stdio.h>
 
bool clear_non_ascii_or_utf8(char* str, int length)
{
    int i = 0;
    int nBytes = 0;         //UTF8可用1-6个字节编码, ASCII用一个字节
    unsigned char chr = 0;
    bool bAllAscii = true;  //如果全部都是ASCII,说明不是UTF-8
    int utf8Start = 0;
 
    while (i < length)
    {
        chr = str[i];
        // first is 1
        if ((chr & 0x80) != 0)
            bAllAscii = false;
        if (nBytes == 0)//计算字节数
        {
            if ((chr & 0x80) != 0)
            {
                while ((chr & 0x80) != 0)
                {
                    chr <<= 1;
                    nBytes++;
                }
                if (nBytes < 2 || nBytes > 4) {
                    // non 110x 
                    str[i] = '_'; //第一个字节最少为110x xxxx
                    i++;
                    nBytes = 0;
                    continue;
                    //return false
                }
                utf8Start = i;
                nBytes--;//减去自身占的一个字节
            }
        }
        else//多字节除了第一个字节外剩下的字节
        {
            if ((chr & 0xc0) != 0x80) {
                // non 10xx
                for(; utf8Start < i; utf8Start++) {
                    if ((str[utf8Start] & 0x80) != 0)
                        str[utf8Start] = '_';
                }
                utf8Start = 0;
                if ((str[i] & 0x80) != 0)
                    str[i] = '_';
                i++;
                nBytes = 0;
                continue;
                //return false;//剩下的字节都是10xx xxxx的形式
            }
            nBytes--;
        }
        ++i;
    }
    if (bAllAscii)
        return true;
    if (nBytes > 0) {
        // non complete end
        for(; utf8Start < i; utf8Start++) {
            if ((str[utf8Start] & 0x80) != 0)
                str[utf8Start] = '_';
        }
        utf8Start = 0;
        nBytes = 0;
    }
    return nBytes == 0;
}
 
int main(int argc, char* argv[])
{
    FILE *f = fopen(argv[1], "r");
    char s[3000];

    while (NULL != fgets(s, (int)sizeof(s), f)) {
        clear_non_ascii_or_utf8(s, strlen(s));
        printf("%s\n", s);
    }
 
    return 0;
}
