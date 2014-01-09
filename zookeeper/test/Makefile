
all:server rry1 monitor wexists_test aexists_test awexists_test sync_test sync_test2

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
	rm *.o server monitor exists_test wexists_test aexists_test awexists_test

rry1:rry1.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

rry1.o:rry1.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^



wexists_test:wexists_test.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

wexists_test.o:wexists_test.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

aexists_test:aexists_test.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

aexists_test.o:aexists_test.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

awexists_test:awexists_test.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

awexists_test.o:awexists_test.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

sync_test:sync_test.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

sync_test.o:sync_test.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^

sync_test2:sync_test2.o 
	gcc -g -L/usr/local/lib/ -lzookeeper_mt -o $@ $^ 

sync_test2.o:sync_test2.c
	gcc -g -DTHREADED -I/usr/local/include/zookeeper -o $@ -c $^
