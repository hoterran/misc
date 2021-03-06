#!/usr/bin/env python
#coding: utf-8
import select, socket
import threading

def gen_events(socks):
	while True:
		rdr, wrt, err = select.select(socks, socks, socks, 0.1)
		for r in rdr:
			yield "read", r
		for w in wrt:
			yield "write", w

def recv_conn(addr):
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	s.bind(addr)
	s.listen(5)
	while True:
		client = s.accept()
		yield client

def acceptor(sockset, addr):
	for c, a in recv_conn(addr):
		sockset.append(c)

def main():
	clientset = []
	data = {}
	acc_thr = threading.Thread(target=acceptor,
			args=(clientset, ("", 12000)))
	acc_thr.setDaemon(True)
	acc_thr.start()

	for evt, s in gen_events(clientset):
		if evt == 'read':
			buf = s.recv(1024)
			if not buf:
				print 'close' ,s
				s.close()
				clientset.remove(s)
			else:
				if data.has_key(s):
					data[s] = data[s] + buf
				else:
					data[s] = buf

		elif evt == 'write':
			slen = 100
			if len(data[s]) > slen:
				print "#" + data[s][:slen - 1]
				s.send(data[s][:slen - 1])
				data[s] = data[s][slen:]
if __name__ == "__main__":
	main()
