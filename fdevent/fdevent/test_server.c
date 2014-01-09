/*
fdevent - 方便的跨平台IO多路复用接口
主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
author: ideawu
version: 1.0
author uri: http://www.ideawu.net/
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include "fdevent.h"

static int create_listener(int port){
	int sock;
	struct sockaddr_in addr;

	if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1){
		return -1;
	}

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

	if(bind(sock, (struct sockaddr *)&addr, sizeof(addr)) == -1){
		return -1;
	}

	if(listen(sock, 1024) == -1){
		return -1;
	}

	return sock;
}


int listener;
int port;

struct fdevents *evs;

static void init(){
	port = 10210;

	listener = create_listener(port);
	if(listener == -1){
		perror("Create listener socket error: ");
		exit(0);
	}
	printf("Create listener socket fd: %d\n", listener);
}

static void run(){
	struct fdevents *evs;
	struct fdevent *fde;
	struct fdevent ev;
	int ret;

	int i, nfds, new_sock;
	struct sockaddr_in addr;
	socklen_t addr_len;
	char buf[1024];
	int fd;

	evs = fdevents_init(1024);
	assert(evs);
#ifdef FDEVENT_USE_EPOLL
	printf("create epoll fd: %d\n", evs->epoll_fd);
#endif

	ev.fd = listener;
	ev.flags = FDEVENT_IN;
	ev.data.ptr = NULL;
	fdevents_add(evs, &ev);

	while(1) {
		nfds = fdevents_wait(evs, 1000);
		if(nfds == 0){
			//printf("timeout\n");
			continue;
		}

		for(i = 0; i < nfds; i++) {
			fde = evs->events[i];

			if(!(fde->flags & FDEVENT_IN)){
				printf("Fatal error: not FDEVENT_IN!\n");
				exit(0);
			}

			fd = fde->fd;
			if(fd == listener) {
				new_sock = accept(listener, (struct sockaddr *)&addr, &addr_len);
				if(new_sock == -1){
					perror("Accept error");
					continue;
				}
				ev.fd = new_sock;
				ev.flags = FDEVENT_IN;
				ev.data.ptr = NULL;
				ret = fdevents_add(evs, &ev);
				if(ret == -1){
					perror("无法注册事件监听");
				}
				printf("Accept fd: %d\n", new_sock);
			}else{
				int nbytes;
				while((nbytes = read(fd, buf, 1024)) == -1 && errno == EINTR){
				}
				printf("Read fd: %d, bytes: %d\n", fd, nbytes);
				if(nbytes <= 0){
					ev.fd = fd;
					fdevents_del(evs, &ev);
					close(fd);
					if(nbytes == -1){
						perror("");
					}
					printf("Close fd: %d\n", fd);
				}
				write(fd, buf, nbytes);
			}
		}
	}

	fdevents_free(evs);
}


int main(int argc, char **argv){
	init();
	run();
	return 0;
}
