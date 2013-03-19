#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "ae.h"

#define PTHREAD_NUM  2

void sock_read(aeEventLoop *ae, int fd, void *privdata, int mask);

void sock_write(aeEventLoop *ae, int fd, void *privdata, int mask) {

    char buf[100];

    snprintf(buf, sizeof(buf), "%d read:%s write:%s\n", pthread_self(), (char*)privdata, "haha");

    send(fd, buf, strlen(buf), 0);
    fprintf(stdout, "%s\n", buf);
    aeDeleteFileEvent(ae, fd, AE_WRITABLE);
    free(privdata);
    aeCreateFileEvent(ae, fd, AE_READABLE, sock_read, NULL);
}

void sock_read(aeEventLoop *ae, int fd, void *privdata, int mask) {
    char buf[255];
    int len;
    char *s = privdata;

    len = recv(fd, buf, sizeof(buf)-1, 0);

    if (len == -1) {
        perror("recv error\n");

        if (errno != EAGAIN && errno != EINTR)
        {
            close(fd);
        }

        return;
    } else if (len == 0) {
        close(fd);
        fprintf(stderr, "Connection closed\n");
        return;
    }

    buf[len] = '\0';

    //fprintf(stdout, "Read: %s\n", buf);

    if (NULL == s) {
        s = malloc(len + 1);
        memcpy(s, buf, len + 1);
    } else {
        s = realloc(s, strlen(s) + len);  
        memcpy(s + strlen(s), buf, len + 1);
    }

    /* data large then 100 then can write*/
    aeCreateFileEvent(ae, fd, AE_WRITABLE, sock_write, s);
    aeDeleteFileEvent(ae, fd, AE_READABLE);
}

void sock_accept(aeEventLoop *ae, int fd, void *privdata, int mask)
{
    int *efd = privdata;
    int ret;
    
    struct sockaddr addr;
    socklen_t len = sizeof(addr);

    uint64_t s = accept(fd, &addr, &len);
    if (s == -1) {
        perror("accept error\n");
        return;
    }

    // only send to first thread
    int i = (rand() % PTHREAD_NUM);

    ret = write(efd[i], &s, sizeof(s));

    fprintf(stdout, "accept socket: %d, send to %d, %d \n", (int)s, efd[i], ret);
    printf("%s", strerror(errno));
}

void thread_accept(aeEventLoop *ae, int efd, void *privdata, int mask)
{
    uint64_t fd;
    int ret;

    ret = read(efd, &fd, sizeof(fd));

    printf("%d recv %d\n", pthread_self(), (int)fd);

    aeCreateFileEvent(ae, (int)fd, AE_READABLE, sock_read, NULL);
}

void* thread(void *fd) {
   
    int efd = *(int*)fd;

    printf("%d - %d\n", pthread_self(), efd);

    aeEventLoop *ae = aeCreateEventLoop();

    aeCreateFileEvent(ae, efd, AE_READABLE, thread_accept, NULL);

    aeMain(ae);
}

int
main (int argc, char **argv)
{
    aeEventLoop *ae = aeCreateEventLoop();

    int fd = net_listen(10000);
    if (fd < 0) {
        exit(1); 
    }

    pthread_t pid;
    int ret = 0;
    int i = 0;
    int efd[PTHREAD_NUM];

    for (i = 0; i < PTHREAD_NUM; i++) {
        efd[i] = eventfd(0, 0);
        ret = pthread_create(&pid, NULL, thread, &efd[i]);
        printf("thread and efd %d - %ld \n", efd[i], pid);
    }

    aeCreateFileEvent(ae, fd, AE_READABLE, sock_accept, &efd);

    aeMain(ae);

    return (0);
}
 
