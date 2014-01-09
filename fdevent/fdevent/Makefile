#fdevent - 方便的跨平台IO多路复用接口
#主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
#author: ideawu
#version: 1.0
#author uri: http://www.ideawu.net/

CFLAGS = -g -Wall

all: test

fdevent.o:
	gcc ${CFLAGS} -DFDEVENT_USE_SELECT -c -o fdevent.o fdevent.c

test: test_client test_server

test_client: fdevent.o test_client.c
	gcc ${CFLAGS} -o test_client test_client.c

test_server: fdevent.o test_server.c
	gcc ${CFLAGS} -DFDEVENT_USE_SELECT -o test_server test_server.c fdevent.c

clean:
	rm -f fdevent.o test_client test_client.exe test_server test_server.exe
