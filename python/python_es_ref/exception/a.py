#!/usr/bin/env python
#coding: utf-8

def bbbb():
	try:
		print aaaa
	except:
		raise
def kkk():
	try:
		bbbb()
	except:
		raise

if __name__ == "__main__":
	kkk()
