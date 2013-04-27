#!/usr/bin/env python
#coding: utf-8
import sys, os
from socket import *
import datetime

def main():
	s = socket(AF_INET, SOCK_STREAM)
	s.bind(('', 8888))
	s.listen(5)

	while True:
		client, addr = s.accept()
		print "%s come" % str(addr)
				
		client.send("fuck %s" % str(datetime.datetime.now()))

		client.close()

if __name__ == "__main__":
	main()
