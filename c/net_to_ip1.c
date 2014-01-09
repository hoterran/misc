#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

    struct in_addr s;
    s.s_addr = strtoul(argv[1], NULL, 10);
    printf("%s\n",inet_ntoa(s));
    return 0;
}
