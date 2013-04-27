#!/usr/bin/env python
#coding: utf-8
import os
import sys

class c(object):
	def __init__(self):
		print 'init'
	def __del__(self):
		print 'del'

if __name__ == "__main__":
	a = c()

	#a = 1 #del
	pid = os.fork()

	if pid == 0:
		#sys.exit(0) # del
		os._exit(1)  # not del
	
	raw_input()
