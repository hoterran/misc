#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    int fd = open("sparse.file", O_RDWR|O_CREAT);
    lseek(fd, 10204, SEEK_CUR);
    write(fd, "abc", 3);
    lseek(fd, 1020400, SEEK_CUR);
    write(fd, "efg", 3);

    return 0;
}
