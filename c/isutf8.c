#include <string.h>
#include <stdio.h>
 
bool IsUTF8String(const char* str, int length)
{
    int i = 0;
    int nBytes = 0;         //UTF8可用1-6个字节编码, ASCII用一个字节
    unsigned char chr = 0;
    bool bAllAscii = true;  //如果全部都是ASCII,说明不是UTF-8
 
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
                if (nBytes < 2 || nBytes > 6)
                    return false;//第一个字节最少为110x xxxx
                nBytes--;//减去自身占的一个字节
            }
        }
        else//多字节除了第一个字节外剩下的字节
        {
            if ((chr & 0xc0) != 0x80)
                return false;//剩下的字节都是10xx xxxx的形式
            nBytes--;
        }
        ++i;
    }
    if (bAllAscii)
        return false;
    return nBytes == 0;
}
 
int main()
{
    printf("%d\n", IsUTF8String("cc", strlen("cc")));
    printf("%d\n", IsUTF8String("曹操", strlen("曹操")));
 
    return 0;
}
