#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

    struct in_addr s;
    inet_aton(argv[1], &s);
    printf("%u\n", s.s_addr);
    return 0;
}
