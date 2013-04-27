#!/usr/bin/env python
#coding: utf-8
import threading
import time

def clock(interval):
	while True:
		print 'aaaa'
		time.sleep(interval)

def main():
	t = threading.Thread(target=clock, args=(3,))
	t.daemon = True
	t.start()
	t.join()

if __name__ == "__main__":
	main()
