/*
fdevent - 方便的跨平台IO多路复用接口
主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
author: ideawu
version: 1.0
author uri: http://www.ideawu.net/
*/
#ifdef FDEVENT_USE_EPOLL

#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include "fdevent.h"

struct fdevents *fdevents_init(int max_fds){
	struct fdevents *evs;
	int epfd;
	int i;

	epfd = epoll_create(max_fds);
	if(epfd == -1){
		return NULL;
	}

	evs = calloc(1, sizeof(*evs));
	if(!evs){
		return NULL;
	}
	evs->type = FDEVENTS_TYPE_EPOLL;
	evs->max_fds = max_fds;
	evs->cur_fds = 0;
	evs->epoll_fd = -1;

	evs->events = calloc(max_fds, sizeof(*evs->events));
	if(!evs->events){
		fdevents_free(evs);
		return NULL;
	}

	evs->ev_array = calloc(max_fds, sizeof(*evs->ev_array));
	if(!evs->ev_array){
		fdevents_free(evs);
		return NULL;
	}
	for(i = 0; i < max_fds; i++){
		evs->ev_array[i].fd = -1;
	}

	evs->epoll_fd = epfd;
	evs->epoll_events = calloc(max_fds, sizeof(*evs->epoll_events));
	if(!evs->epoll_events){
		fdevents_free(evs);
		return NULL;
	}

	return evs;
}

void fdevents_free(struct fdevents *evs){
	if(evs->epoll_fd >= 0){
		close(evs->epoll_fd);
	}
	if(evs->epoll_events){
		free(evs->epoll_events);
	}
	if(evs->ev_array){
		free(evs->ev_array);
	}
	if(evs->events){
		free(evs->events);
	}
	free(evs);
}

int fdevents_add(struct fdevents *evs, struct fdevent *ev){
	struct epoll_event epe;
	struct fdevent *fde;
	int ret;

	assert(ev->fd >= 0 && ev->fd < evs->max_fds);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == -1);

	epe.data.ptr = fde;
	epe.events = 0;
	if(ev->flags & FDEVENT_IN)  epe.events |= EPOLLIN;
	if(ev->flags & FDEVENT_OUT) epe.events |= EPOLLOUT;

	ret = epoll_ctl(evs->epoll_fd, EPOLL_CTL_ADD, ev->fd, &epe);
	if(ret == -1){
		return -1;
	}

	evs->cur_fds++;
	fde->fd = ev->fd;
	fde->data = ev->data;
	fde->s_flags = ev->flags;

	return 0;
}

int fdevents_del(struct fdevents *evs, struct fdevent *ev){
	struct fdevent *fde;
	int ret;

	assert(ev->fd >= 0 && ev->fd < evs->max_fds);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == ev->fd);

	ret = epoll_ctl(evs->epoll_fd, EPOLL_CTL_DEL, ev->fd, NULL);
	if(ret == -1){
		return -1;
	}

	evs->cur_fds--;
	fde->fd = -1;

	return 0;
}

int fdevents_mod(struct fdevents *evs, struct fdevent *ev){
	struct epoll_event epe;
	struct fdevent *fde;
	int ret;

	assert(ev->fd >= 0 && ev->fd < evs->max_fds);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == ev->fd);

	epe.data.ptr = fde;
	epe.events = 0;
	if(ev->flags & FDEVENT_IN)  epe.events |= EPOLLIN;
	if(ev->flags & FDEVENT_OUT) epe.events |= EPOLLOUT;

	ret = epoll_ctl(evs->epoll_fd, EPOLL_CTL_MOD, ev->fd, &epe);
	if(ret == -1){
		return -1;
	}
	fde->data = ev->data;
	fde->s_flags = ev->flags;

	return 0;
}

int fdevents_set(struct fdevents *evs, int fd, int flags){
	struct fdevent *fde;
	struct fdevent ev;

	assert(fd >= 0 && fd < evs->max_fds);
	fde = &evs->ev_array[fd];
	assert(fde->fd == fd);

	ev.fd = fd;
	ev.flags = fde->s_flags | flags;
	ev.data = fde->data;

	return fdevents_mod(evs, &ev);
}

int fdevents_clr(struct fdevents *evs, int fd, int flags){
	struct fdevent *fde;
	struct fdevent ev;

	assert(fd >= 0 && fd < evs->max_fds);
	fde = &evs->ev_array[fd];
	assert(fde->fd == fd);

	ev.fd = fd;
	ev.flags = fde->s_flags & ~flags;
	ev.data = fde->data;

	return fdevents_mod(evs, &ev);
}

int fdevents_wait(struct fdevents *evs, int timeout_ms){
	struct fdevent *fde;
	struct epoll_event *epe;
	int i, nfds;

	nfds = epoll_wait(evs->epoll_fd, evs->epoll_events, evs->max_fds, timeout_ms);
	for(i = 0; i < nfds; i++){
		epe = &evs->epoll_events[i];
		fde = epe->data.ptr;

		/* 将接口相关的信号转换为fdevent */
		fde->flags = FDEVENT_NONE;
		if(epe->events & EPOLLIN)  fde->flags |= FDEVENT_IN;
		if(epe->events & EPOLLPRI) fde->flags |= FDEVENT_PRI;
		if(epe->events & EPOLLOUT) fde->flags |= FDEVENT_OUT;
		if(epe->events & EPOLLHUP) fde->flags |= FDEVENT_HUP;
		if(epe->events & EPOLLERR) fde->flags |= FDEVENT_ERR;

		/* 将fdevent添加到就绪列表中 */
		evs->events[i] = fde;
	}

	return nfds;
}

#endif
