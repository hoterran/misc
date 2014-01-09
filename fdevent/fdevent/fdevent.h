/*
fdevent - 方便的跨平台IO多路复用接口
主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
author: ideawu
version: 1.0
author uri: http://www.ideawu.net/
*/
#ifndef UTIL__FDEVENT_H
#define UTIL__FDEVENT_H

#ifdef FDEVENT_USE_EPOLL
#	include <sys/epoll.h>
#endif

#ifdef FDEVENT_USE_SELECT
#	include <sys/select.h>
#endif

#define FDEVENTS_TYPE_SELECT	1
#define FDEVENTS_TYPE_EPOLL		2

#define FDEVENT_NONE	(0)
#define FDEVENT_IN		(1<<0)
#define FDEVENT_PRI		(1<<1)
#define FDEVENT_OUT		(1<<2)
#define FDEVENT_HUP		(1<<3)
#define FDEVENT_ERR		(1<<4)


struct fdevent{
	int fd;
	int flags;
	int s_flags; /* private: already set flags */

	struct{
		int num;
		void *ptr;
	}data;
};

struct fdevents{
	int type;

	int max_fds; /* max number of fds */
	int cur_fds; /* number of fds added to this fdevents */
	struct fdevent **events;
	struct fdevent *ev_array; /* private field */

#ifdef FDEVENT_USE_EPOLL
	int epoll_fd;
	struct epoll_event *epoll_events;
#endif

#ifdef FDEVENT_USE_SELECT
	int select_maxfd;
	fd_set select_readset;
	fd_set select_writeset;

	fd_set select_readset__;
	fd_set select_writeset__;
#endif

};

/*
*/
struct fdevents *fdevents_init(int max_fds);
void fdevents_free(struct fdevents *evs);

/* 添加文件描述符要监听的事件. */
int fdevents_add(struct fdevents *evs, struct fdevent *ev);

/* 删除文件描述符要监听的事件. */
int fdevents_del(struct fdevents *evs, struct fdevent *ev);

/* 修改文件描述符要监听的事件. */
int fdevents_mod(struct fdevents *evs, struct fdevent *ev);

/* 类似 FD_SET, 但fd在调用前必须已经被监听 */
int fdevents_set(struct fdevents *evs, int fd, int flags);

/* 类似 FD_CLR, 但fd在调用前必须已经被监听 */
int fdevents_clr(struct fdevents *evs, int fd, int flags);

/* 等待监听队列监听的事件, 返回就绪的文件描述符数目. */
int fdevents_wait(struct fdevents *evs, int timeout_ms);

#endif
