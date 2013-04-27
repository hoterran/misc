#!/usr/bin/env python
#coding: utf-8

import multiprocessing
import time

def clock(interval):
	while True:
		print 'time %s' % time.ctime()
		time.sleep(interval)

if __name__ == "__main__":
	p = multiprocessing.Process(target=clock, args=(5,))
	p.daemon = True
	p.start()

	time.sleep(10)
