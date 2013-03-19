#main thread
1. listen
2. create threads and EFDs
3. create eventloop, add listenfd in it
3. accept clientfd, choose a EFD(thread), write it


#thread
1. create eventloop, add EFD in it
2. read EFD, receive clientfd
3. add clientfd in eventloop
4. read clientfd, receive data

