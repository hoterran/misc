/*
fdevent - 方便的跨平台IO多路复用接口
主要想法来自 epoll 和 lighttd 的 fdevent, 接口的使用几乎和 epoll 一样.
author: ideawu
version: 1.0
author uri: http://www.ideawu.net/
*/
#include "fdevent.h"

#ifdef FDEVENT_USE_EPOLL
#	include "fdevent_epoll.c"
#endif

#ifdef FDEVENT_USE_SELECT
#	include "fdevent_select.c"
#endif
