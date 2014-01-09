
class a:
    def __init__(self):
        self.n = 1
    def fun1(self):
        return self.n

class b(a):
    def __init__(self):
        self.n = 2

    def fun1(self):
        return self.n

    def fun2(self):
        return self.n

    @staticmethod
    def fun3():
        return 3

m = b()
print m.fun1()
print m.fun2()
print b.fun3()

