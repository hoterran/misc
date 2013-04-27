#!/usr/bin/env python
#coding: utf-8

def func(str):
	"""
	split func

	>>> func("abc")
	['abc']
	>>> func("abcd")
	['a2bcd']
	"""
	return str.split()

def main():
	import doctest
	doctest.testmod()
	#print func("abc")
	#doctest.DocTestSuite()

if __name__ == "__main__":
	main()
