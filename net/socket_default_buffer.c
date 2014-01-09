#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>

int main(int argc, char *argv[])
{
    int sockfd;
    int bufsize;
    int len = sizeof(int);

    sockfd = socket(AF_INET, SOCK_STREAM, 0);

    if (getsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, (void *)&bufsize, &len) == 0)
        printf("default: so_sndbuf = %d\n", bufsize);

    if (getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (void *)&bufsize, &len) == 0)
        printf("default: so_rcvbuf = %d\n", bufsize);

}
