#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>

//use iptables -A INPUT -s 10.202.66.9 -j DROP

int readClient(int fd) {
    char buffer[1024] = {};
    int nbytes;

    nbytes = read(fd, buffer, sizeof(buffer));
    if (nbytes < 0) {
        printf("keepalive go here?, %d\n", errno);
      	return 0; 
    } else if (nbytes == 0) {
        printf(" = 0\n");
	return 0;
    } else {
        printf("data %s", buffer);
        return nbytes;
    }
}

int main(int argc, char *argv[1])
{
    int s;
    int optval;
    socklen_t optlen = sizeof(optval);

    /* Create the socket */
    if((s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
      perror("socket()");
      exit(EXIT_FAILURE);
    }

    /* Check the status for the keepalive option */
    if(getsockopt(s, SOL_SOCKET, SO_KEEPALIVE, &optval, &optlen) < 0) {
      perror("getsockopt()");
      close(s);
      exit(EXIT_FAILURE);
    }
    printf("SO_KEEPALIVE is %s\n", (optval ? "ON" : "OFF"));

    /* Set the option active */
    optval = 1;
    optlen = sizeof(optval);
    if(setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, &optval, optlen) < 0) {
      perror("setsockopt()");
      close(s);
      exit(EXIT_FAILURE);
    }
    printf("SO_KEEPALIVE set on socket\n");

    /* Check the status again */
    if(getsockopt(s, SOL_SOCKET, SO_KEEPALIVE, &optval, &optlen) < 0) {
      perror("getsockopt()");
      close(s);
      exit(EXIT_FAILURE);
    }
    printf("SO_KEEPALIVE is %s\n", (optval ? "ON" : "OFF"));

    struct sockaddr_in addr;    
    memset(&addr,0, sizeof(addr)); 
    addr.sin_family = AF_INET;
    addr.sin_port = htons(atoi(argv[1]));
    addr.sin_addr.s_addr = 0;

    if (bind(s, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        perror("bind error\n");
        return -1; 
    }   
    
    if (listen(s, 20) == -1) {
        perror("listen error\n");    
        return -1; 
    }   
    
    struct sockaddr_in client_address;
    socklen_t client_len = sizeof(struct sockaddr_in);
    fd_set active_fd_set, read_fd_set, exception_fd_set, write_fd_set;
   
    FD_ZERO(&active_fd_set);
    FD_SET(s, &active_fd_set);
    int i;
    int ret;

    while(1) {
        read_fd_set = active_fd_set;

        ret =  select (FD_SETSIZE, &read_fd_set, NULL, NULL, NULL);

        if (ret < 0) {
            perror("select");
            exit(EXIT_FAILURE);
        }

        for (i = 0; i < FD_SETSIZE; ++i) {
            if (FD_ISSET(i, &read_fd_set)) {
                printf(" read %d", i);
                if (i == s) {
                    int clientFd = accept(s, NULL, NULL);
                    FD_SET(clientFd, &active_fd_set);
                    printf(" accept %d\n", clientFd);
                } else {
                    if (readClient(i) <= 0) {
                        close(i);
                        FD_CLR(i, &active_fd_set);
                    }
                }
            }
        }
    }

   close(s);

   exit(EXIT_SUCCESS);
}

