#!/usr/bin/env python
#coding: utf-8

import multiprocessing

def adder(pipe):
	server_p, client_p = pipe
	client_p.close()

	while True:
		try:
			x, y = server_p.recv()
		except EOFError:
			break
		result = x + y
		server_p.send(result)
	
	print("Server done")

def main():
	(server_p, client_p) = multiprocessing.Pipe()

	adder_p = multiprocessing.Process(target=adder, args=((server_p, client_p), ))
	adder_p.start()

	#only use one pipe send and recv
	server_p.close()

	client_p.send((3, 4))
	print(client_p.recv())

	client_p.send(('Hello', 'World'))
	print(client_p.recv())

	client_p.close()

	adder_p.join()

if __name__ == "__main__":
	main()
