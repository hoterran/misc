#!/usr/bin/env python
#coding: utf-8

import multiprocessing
import time
import os

def consumer(input_q, l, c):
    while True:
        item = input_q.get()
        print("%d consumer %s " % (os.getpid(), item))
        l.acquire()
        c.value = c.value + 1
        l.release()
        input_q.task_done()

def producer(sequence, output_q):
    for item in sequence:
        print("producer ", item)
        output_q.put(item)

if __name__ == "__main__":
    #Queue in two process
    q = multiprocessing.JoinableQueue()
    l = multiprocessing.Lock()
    # share resource
    c = multiprocessing.Value('d', 0.0)

    sequence = []
    for i in range(100000):
        sequence.append(i)

    producer(sequence, q)

    ps = []
    for i in range(3):
        p = multiprocessing.Process(target=consumer, args=(q, l, c))
        p.daemon = True
        ps.append(p)

    for i in range(3):
        ps[i].start()

    q.join()
    print c.value

