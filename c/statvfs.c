#include <sys/statvfs.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
    struct statvfs sv;
    statvfs(argv[1], &sv);

    printf("f_bsize(file system block size):            %lu\n", sv.f_bsize);
    printf("f_frsize(fragment size):                    %lu\n", sv.f_frsize);
    printf("f_blocks(size of fs in f_frsize units):     %lu\n", sv.f_blocks);
    printf("f_bfree(free blocks):                       %lu\n", sv.f_bfree);
    printf("f_bavail(free blocks for unprivileged users):%lu\n", sv.f_bavail);
    printf("f_files(inodes):                            %lu\n", sv.f_files);
    printf("f_ffree(free inodes):                       %lu\n", sv.f_ffree);
    printf("f_favail(free inodes for unprivileged users):%lu\n", sv.f_favail);

    printf("f_fsid(file system ID):                         %lu\n", sv.f_fsid);
    printf("f_flag(mount flags):                            %lu\n", sv.f_flag);
    printf("f_namemax(maximum filename length):             %lu\n", sv.f_namemax);
}
