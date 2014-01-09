#!/usr/bin/env python
#coding: utf-8
import socket

def main():
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect(('localhost', 9000))

	tm = s.recv(1024)
	s.close()
	print tm

if __name__ == "__main__":
	main()
