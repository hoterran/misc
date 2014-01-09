#!/usr/bin/env python
#coding: utf-8

from follow import follow

def grep(pattern, lines):
	for line in lines:
		if pattern in line:
			yield line

def main():
	logfile = open("access-log")
	loglines = follow(logfile)
	pylines = grep("aa", loglines)

	for line in pylines:
		print line,

if __name__ == "__main__":
	main()
