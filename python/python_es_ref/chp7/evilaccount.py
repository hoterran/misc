#!/usr/bin/env python

import random
from account import *

class EvilAccount(Account):
	"""
	def inquiry(self):
		if random.randint(0, 4) == 1:
			return self.balance * 1.10
		else:
			return self.balance
	"""
	def __init__(self, name, balance, evilfactor):
		Account.__init__(self, name, balance)
		self.evilfactor = evilfactor

	def inquiry(self):
		if random.randint(0, 4) == 1:
			return self.balance * self.evilfactor
		else:
			return self.balance
