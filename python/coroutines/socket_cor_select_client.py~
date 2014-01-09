#!/usr/bin/env python
#coding: utf-8

from socket import *
import sys
import time

def main():
	messages = ['This is the message.', 
			'It will be sent ',
			'in parts.', ]

	server_address = ('localhost', 9999)

	socks = [socket(AF_INET, SOCK_STREAM),
		socket(AF_INET, SOCK_STREAM)]
	
	for s in socks:
		s.connect(server_address)

	while True:

		for message in messages:
			for s in socks:
				print '%s: send %s' % (s.getsockname(), message)
				s.send(message)

		time.sleep(2)

if __name__ == "__main__":
	main()
