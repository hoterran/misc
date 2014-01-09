#!/usr/bin/env python

def foo1(x):
    x = 1

def foo3(x):
    x = "def"

def foo4(x):
    x.append("def")

x=0
foo1(x)
print x

x="abc"
foo3(x)
print x

x=["abc"]
foo4(x)
print x
