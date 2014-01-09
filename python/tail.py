#!/usr/bin/env python
"""
	tail file get last n lines

"""
import os
import sys

def min(a, b):
	if a > b:
		return b
	else:
		return a

def tail(f, lines=5):
	f.seek(0, 2)
	fbytes = f.tell()
	lines_found = 0
	block = 0
	is_head = 0
	while(1):
		block = block + 1
		f.seek( -1 * min(1024 * block, fbytes), 2)
		if f.tell() == 0:
			is_head = 1	
		data = f.read(1024)
		lines_found = lines_found + data.count('\n')
		print "-----%d--%d--" % (lines_found, is_head)
		if lines_found < lines:
	                if is_head == 1:
			#file head
				f.seek( -1 * min(1024 * block, fbytes), 2)
                       		break
			else:
			#rewind again
				continue
		elif lines_found > lines:
			#lines two much
			l = lines_found - lines
			f.seek( -1 * min(1024 * block, fbytes), 2)
			while(l > 0):
				#read more line 
				f.readline()
				l -=1
			break
		else:
			f.seek( -1 * min(1024 * block, fbytes), 2)
			break

	#now is positon
	l = list(f.readlines())
	print l

def main():
	f = open("1.log", "r")
	tail(f, 10)
	f.close()

if __name__ == "__main__":
	main()

