#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <signal.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/epoll.h>

#define OK 0
#define ERROR -1

int get_ip_from_hostname (const char * hostname, char *ip)
{
    int ret, herrno;
    char buf[8192] = {}; 
    struct hostent hostinfo, *phost= NULL;
              
    ret = gethostbyname_r(hostname, &hostinfo, buf, sizeof(buf), &phost, &herrno);
              
    if (NULL == phost)
    {         
        return ERROR;
    }         
              
    inet_ntop(AF_INET,(struct in_addr*)hostinfo.h_addr_list[0],ip,INET_ADDRSTRLEN);
              
    return OK;
} 

int main(int argc, char* argv[]) {
    char ip[15] = {};
    get_ip_from_hostname(argv[1], ip);
    printf("%s-%s\n", argv[1], ip);
    return 0;

}
