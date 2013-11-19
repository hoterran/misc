#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>


int fdArray[100000] = {};
int i = 0;

int c (char *ip, int port) {

    int fd;
    struct sockaddr_in server_address;
 
    fd = socket(AF_INET, SOCK_STREAM, 0);
    fdArray[i] = fd;

    memset(&server_address,0,sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = inet_addr(ip);
    server_address.sin_port = htons(port);

    int result = connect(fd,(struct sockaddr*)&server_address,sizeof(server_address));
    if (result != 0) {
        printf("failure %s, %d\n", strerror(errno), errno);
    }

}

int main(int argc, char *argv[])  {
    while(1) {
        printf("gogo\n");
        c(argv[1], atoi(argv[2]));
        i++;
        if (i % 1000 == 0) {
            printf("%d\n", i);
            sleep(1);
        }
    }
    return;
}
