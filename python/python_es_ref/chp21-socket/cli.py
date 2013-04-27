#!/usr/bin/env python
#coding: utf-8
import sys,os
from socket import *
import time

def main():
	s = socket(AF_INET, SOCK_STREAM)
	s.connect(('localhost', 8888))

	tm = s.recv(1024)
	s.close()
	print tm

if __name__ == "__main__":
	main()
