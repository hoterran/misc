#!/usr/bin/env python
#coding: utf-8

def countdown(n):
	print "Counting down from"
	while n > 0:
		yield n
		n -= 1
	print "Done counting down"

def main():
	d = countdown(10)
	for i in d:
		print i

if __name__ == "__main__":
	main()
