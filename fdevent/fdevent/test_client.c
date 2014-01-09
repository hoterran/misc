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
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>

static int create_client(int port){
	int sock;
	struct sockaddr_in addr;

	if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1){
		return -1;
	}

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);

	if(connect(sock, (struct sockaddr *)&addr, sizeof(addr)) == -1){
		return -1;
	}

	return sock;
}


int main(int argc, char **argv){
	int sock;
	char str[1024];
	char buf[1024];
	int str_len, len;
	int i;

	sock = create_client(10210);

	if(sock == -1){
		perror("create_client error.");
		exit(0);
	}

	str_len = strlen(str);

	for(i = 0; i < 100000; i++){
		str_len = sprintf(str, "echo: %d\n", i);

		write(sock, str, str_len);
		len = read(sock, buf, 1024);
		buf[len] = '\0';
		fprintf(stderr, buf);
		usleep(100 * 1000);
	}

	return 0;
}
