#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include "evbuffer.h"
#include "ae.h"

#define HEADER_LENGTH  3

int log_3(aeEventLoop *ae, long long id, void *clientData) {
    
    fprintf(stdout, "log_3 %ld\n", time(NULL));
    sleep(2);
    return 1000;
}

int log_2(aeEventLoop *ae, long long id, void *clientData) {
    
    fprintf(stdout, "log_2 %ld\n", time(NULL));
    sleep(1);
    return 1000;
}

int log_1(aeEventLoop *ae, long long id, void *clientData) {
    
    fprintf(stdout, "log_1 %ld\n", time(NULL));
    sleep(1);
    return 1000;
}

int
main (int argc, char **argv)
{
    setbuf(stdout, 0);
    aeEventLoop *ae = aeCreateEventLoop();

    int id1 = aeCreateTimeEvent(ae, 0, log_1, NULL, NULL);
    int id2 = aeCreateTimeEvent(ae, 0, log_2, NULL, NULL);
    int id3 = aeCreateTimeEvent(ae, 0, log_3, NULL, NULL);

    aeMain(ae);

    return (0);
}
 
