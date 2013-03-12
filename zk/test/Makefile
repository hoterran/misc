
all:server rry1 monitor exists_test

monitor:monitor.o
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

monitor.o:monitor.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

server:server.o
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

server.o:server.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

.PHONY:clean

clean:
	rm server.o server rry1

rry1:rry1.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

rry1.o:rry1.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^



wexists_test:wexists_test.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

wexists_test.o:wexists_test.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^
