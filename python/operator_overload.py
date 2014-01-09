#!/usr/bin/env python
#coding: utf-8

class bt:
    def __init__(self, num):
        self._num = num
        self.l = ['a', 'b', 'c']
        self.d = {"aaaa":1111, "bbbb": 222}

    def __add__(self, other):
        return self._num + other

    def __str__ (self):
        return " this is str"

    def __hash__(self):
        return 1000

    def __getattr__(self, name):
        if self.d.has_key(name):
            return self.d[name]
        else:
            raise AttributeError

    def __setattr_(self, name, value):
        self.d[name] = value
        return True

    def __getitem__(self, y):
        return self.l.__getitem__(y)

    def __setitem__(self, i, y):
        return self.l.__setitem__(i, y)

    def __len__(self):
        return len(self.l)

    def __contains__(self, elt):
        return self.d.has_key(elt)

    @property
    def num(self):
        return self._num

def main():
    a = bt(1)
    
    print a
    print str(a)
    
    print hash(a)
    print getattr(a, "aaaa")
    print "aaa" in a

    b = bt(10)
    print a + 10
    print len(a)

    c = bt(3)
    c[0] = "aaa"
    c[1] = "bb"
    print c[1], c[0]

    print c.num

if __name__ == "__main__":
    main()
