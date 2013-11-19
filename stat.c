#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
    struct stat s;
    stat(argv[1], &s);

    printf("dev_t(ID of device containing file):    %d\n", s.st_dev);
    printf("ino_t(inode number):                    %d\n", s.st_ino);
    printf("mode_t(protection)                      %d\n", s.st_mode);
    printf("nlink_t(number of hard links)           %d\n", s.st_nlink);
    printf("uid_t( user ID of owner)                %d\n", s.st_uid);
    printf("gid_t(group ID of owner)                %d\n", s.st_gid);
    printf("dev(device ID (if special file))        %d\n", s.st_rdev);
    printf("st_size( total size, in bytes)     %u\n", s.st_size);
    printf("st_blkssize(blocksize for file system I/O)      %u\n", s.st_blksize);
    printf("st_blocks(number of 512B blocks allocated)      %u\n", s.st_blocks);
    printf("st_atime(time of last access)                   %u\n", s.st_atime);
    printf("st_mtime(time of last modification)             %u\n", s.st_mtime);
    printf("st_ctime(time of last status change)            %u\n", s.st_ctime);
}
