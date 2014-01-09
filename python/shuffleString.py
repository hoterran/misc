import sys
import random

"""
    shuffle string or list
    use:
        ./shuffleString.py "abcdefg"
"""

def shuffleList(oldList):
    newList = []
    while oldList:
        i = random.randrange(len(oldList))
        e = oldList[i]
        del oldList[i]
        newList.append(e)
    return newList

print shuffleList(list(sys.argv[1]))
