/*
fdevent - 方便的跨平台IO多路复用接口
主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
author: ideawu
version: 1.0
author uri: http://www.ideawu.net/
*/
#ifdef FDEVENT_USE_SELECT

#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include "fdevent.h"

struct fdevents *fdevents_init(int max_fds){
	struct fdevents *evs;
	int i;

	if(max_fds > FD_SETSIZE){
		max_fds = FD_SETSIZE;
	}

	evs = calloc(1, sizeof(*evs));
	if(!evs){
		return NULL;
	}
	evs->type = FDEVENTS_TYPE_SELECT;
	evs->max_fds = max_fds;
	evs->cur_fds = 0;

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

	FD_ZERO(&evs->select_readset__);
	FD_ZERO(&evs->select_writeset__);
	evs->select_maxfd = -1;

	return evs;
}

void fdevents_free(struct fdevents *evs){
	if(evs->ev_array){
		free(evs->ev_array);
	}
	if(evs->events){
		free(evs->events);
	}
	free(evs);
}

int fdevents_add(struct fdevents *evs, struct fdevent *ev){
	struct fdevent *fde;

	assert(ev->fd >= 0 && ev->fd < evs->max_fds);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == -1);

	if(ev->flags & FDEVENT_IN)  FD_SET(ev->fd, &evs->select_readset__);
	if(ev->flags & FDEVENT_OUT) FD_SET(ev->fd, &evs->select_writeset__);

	evs->cur_fds++;
	fde->fd = ev->fd;
	fde->data = ev->data;
	fde->s_flags = ev->flags;
	if(ev->fd > evs->select_maxfd){
		evs->select_maxfd = ev->fd;
	}

	return 0;
}

int fdevents_del(struct fdevents *evs, struct fdevent *ev){
	struct fdevent *fde;

	assert(ev->fd >= 0 && ev->fd <= evs->select_maxfd);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == ev->fd);

	FD_CLR(ev->fd, &evs->select_readset__);
	FD_CLR(ev->fd, &evs->select_writeset__);

	evs->cur_fds--;
	fde->fd = -1;

	if(ev->fd == evs->select_maxfd){
		do{
			fde--;
		}while(fde->fd == -1 && fde > evs->ev_array);
		evs->select_maxfd = fde->fd;
	}

	return 0;
}

int fdevents_mod(struct fdevents *evs, struct fdevent *ev){
	struct fdevent *fde;

	assert(ev->fd >= 0 && ev->fd <= evs->select_maxfd);
	fde = &evs->ev_array[ev->fd];
	assert(fde->fd == ev->fd);

	FD_CLR(ev->fd, &evs->select_readset__);
	FD_CLR(ev->fd, &evs->select_writeset__);

	if(ev->flags & FDEVENT_IN)  FD_SET(ev->fd, &evs->select_readset__);
	if(ev->flags & FDEVENT_OUT) FD_SET(ev->fd, &evs->select_writeset__);

	fde->data = ev->data;
	fde->s_flags = ev->flags;

	return 0;
}

int fdevents_set(struct fdevents *evs, int fd, int flags){
	struct fdevent *fde;

	assert(fd >= 0 && fd <= evs->select_maxfd);
	fde = &evs->ev_array[fd];
	assert(fde->fd == fd);

	if(flags & FDEVENT_IN)  FD_SET(fd, &evs->select_readset__);
	if(flags & FDEVENT_OUT) FD_SET(fd, &evs->select_writeset__);

	fde->s_flags |= flags;

	return 0;
}

int fdevents_clr(struct fdevents *evs, int fd, int flags){
	struct fdevent *fde;

	assert(fd >= 0 && fd <= evs->select_maxfd);
	fde = &evs->ev_array[fd];
	assert(fde->fd == fd);

	if(flags & FDEVENT_IN)  FD_CLR(fd, &evs->select_readset__);
	if(flags & FDEVENT_OUT) FD_CLR(fd, &evs->select_writeset__);

	fde->s_flags &= ~flags;

	return 0;
}

int fdevents_wait(struct fdevents *evs, int timeout_ms){
	struct timeval tv;
	struct fdevent *fde;
	int i, ret;

	evs->select_readset = evs->select_readset__;
	evs->select_writeset = evs->select_writeset__;

	if(timeout_ms >= 0){
		tv.tv_sec =  timeout_ms / 1000;
		tv.tv_usec = (timeout_ms % 1000) * 1000;

		ret = select(evs->select_maxfd + 1, &evs->select_readset,
			&evs->select_writeset, NULL, &tv);
	}else{
		ret = select(evs->select_maxfd + 1, &evs->select_readset,
			&evs->select_writeset, NULL, NULL);
	}
	if(ret > 0){
		ret = 0;
		for(i = 0; i <= evs->select_maxfd; i++){
			fde = &evs->ev_array[i];

			/* 将接口相关的信号转换为fdevent */
			fde->flags = 0;
			if(FD_ISSET(i, &evs->select_readset))  fde->flags |= FDEVENT_IN;
			if(FD_ISSET(i, &evs->select_writeset)) fde->flags |= FDEVENT_OUT;

			/* 将fdevent添加到就绪列表中 */
			if(fde->flags){
				fde->fd = i;
				evs->events[ret++] = fde;
			}
		}
	}

	return ret;
}

#endif
