#!/usr/bin/env python
#coding: utf-8
from socket import *
import select
import sys,os
import Queue

hostport = ('', 9999)

def main():
	s = socket(AF_INET, SOCK_STREAM)
	s.bind(hostport)
	s.listen(5)

	inputs = [s]
	outputs = []
	mes_que = {}

	while True:
		infds, outfds, errfds = select.select(inputs, outputs, inputs)

		for sock in infds:
			if sock is s:
				#new connect
				new, addr = s.accept()
				print 'new connect %s' % str(addr)
				new.setblocking(0)
				inputs.append(new)

				mes_que[new] = ""
			else:
				#data
				buf = sock.recv(1024)
				if buf:
					print 'recv [%s]' % buf
					mes_que[sock] = mes_que[sock] + buf

					if sock not in outputs:
						outputs.append(sock)
				else:
				#close
					print 'close '
					if sock in outputs:
						outputs.remove(sock)
					inputs.remove(sock)
					sock.close()

					del mes_que[sock]

		for sock in outfds:
			slen = 50
			n = handle_data(mes_que[sock], slen)
			if n == 0:
				continue		
			else:
				s1 = mes_que[sock][:slen - 1 ]
				mes_que[sock] = mes_que[sock][slen:]

				print 'send [%s]' % s1
				sock.send(s1)

def handle_data(s, slen):

	if len(s) > slen:
		return slen 
	return 0

if __name__ == "__main__":
	main()

