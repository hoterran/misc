#!/usr/bin/env python
#coding: utf-8

def h():
	print "first"
	yield 1
	print "second"
	yield 2

def main():
	z = h()
	z.next()

if __name__ == "__main__":
	main()
