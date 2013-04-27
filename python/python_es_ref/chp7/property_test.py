#!/usr/bin/env python
import math
class Circle(object):

	def __init__(self, radius):
		self.radius = radius

	@property
	def area(self):
		return math.pi * self.radius**2

	@property
	def perimeter(self):
		return math.pi * self.radius * 2
