#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


int main() {
    int perms = S_IRUSR|S_IWUSR|S_IWGRP|S_IRGRP|S_IROTH;
    int fd;
    char buf[] = "abc";

    fd=open("o.log",O_CREAT|O_TRUNC|O_RDWR,perms);
    write(fd, buf, sizeof(buf));
    close(fd);

    fd=open("o.log",O_CREAT|O_TRUNC|O_RDWR,perms);
    write(fd, buf, sizeof(buf));
    close(fd);


}
