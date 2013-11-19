#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main(int argc, char *argv[]) {

    net_listen(atoi(argv[1]));
}

int net_listen(int port) {
    int fd; 
    struct sockaddr_in addr;    
 
    fd = socket(AF_INET, SOCK_STREAM, 0); 
    if (fd == -1) {
        perror("socket error\n");
        return -1; 
    }   

    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = 0;
    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        perror("bind error\n");
        return -1; 
    }   

    if (listen(fd, 20) == -1) {
        perror("listen error\n");    
        return -1; 
    }   

    struct  sockaddr_in client_address;
    socklen_t       client_len = sizeof(struct sockaddr_in);

    while(1) {
        accept(fd, (struct sockaddr *)&client_address, &client_len); 
    }   

}
