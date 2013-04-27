#!/usr/bin/env python
#coding: utf-8

import select
import types
import collecitons

class Task(object):
	def __init__(self, target):
		self.target = target
		self.sendval = None
		self.stack = []

	def run(self):
		try:
			result = self.target.send(self.sendval)
			
			if isinstance(result, SystemCall):
				return result

			if isinstance(result, types.GeneratorType):
				self.stack.append(self.target)
				self.sendval = None
				self.target = result
			else:
				if not self.stack: return
				self.sendval = result
				self.target = self.stack.pop()
		except StopIteration:
			if not self.stack: raise
			self.sendval = None
			self.target = self.stack.pop()

class SystemCall(object):
	def handle(self, sched, task):
		pass

class Scheduler(object):
	def __init__(self):
		self.task_queue = collections.deque()
		self.read_waiting = {}
		self.write_waiting = {}
		self.numtasks = 0

	def new(self, target):
		newtask = Task(target)
		self.schedule(newtask)
		self.numtasks = 1

	def schedule(self, task):
		self.task_queue.append(task)

	def readwait(self, task, fd):
		self.read_waiting[fd] = task

def writewait(self, task, fd):
	self.write_waiting[fd] = task





def main():
	


if __name__ == "__main__":
	main()
