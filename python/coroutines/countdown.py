#!/usr/bin/env python
#coding: utf-8

def countdown(n):
	print "Counting down from", n
	while n > 0:
		yield n
		n -= 1
	print "Done counting down"

def main():
	for i in countdown(10):
		print i

if __name__ == "__main__":
	main()
