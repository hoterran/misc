class d:
    def __init__(self):
        self.z = 1
        self.b = 2
        return

    def a(self):
        print "aa"
        return

    def bb(self):
        print "cc"
        return

print dir(d)
print d.__dict__

#class method 1
x = getattr(d, 'a')
q = d()
x(q)

#instance method 2
q2 = d()
y = getattr(q2, 'bb')
y()
