#!/usr/bin/env python
#coding: utf-8

import socket
import glob
import random
import sys
import os

"""
    Manuplicate mysqlpcap binary
    Base hostname mapping random choose redis config file

    use getattr, good example
"""

class MysqlpcapConsole:

    def __init__(self): 
        self.defaultConfigKey = "dg"
        self.keyMap = {"cm5": "qd", "dg":"dg", "ops":"dg"}
        self.filterUser = "aurora,eagleye,replicator,root,xtrabak,Xtrabak"
        self.binary = "./mysqlpcap"

    def __getHostname(self):
        return socket.gethostname()

    def __shuffleList(self, oldList):
        newList = []
        while oldList:
            i = random.randrange(len(oldList))
            e = oldList[i]
            del oldList[i]
            newList.append(e)
        return newList
            
    def __getKeyConfig(self, key):
        files = glob.glob('*redis*.ini')
        # reorder, for random choose
        shuffledFiles = self.__shuffleList(files)
        for f in shuffledFiles:
            if f.find(key) != -1:
                return f
        return None
              
    def __matchKeyMap(self, hostname):
        for key in self.keyMap:
            if hostname.find(key) != -1:
                return self.keyMap[key]
        return self.defaultConfigKey

    def start(self):
        dirname = os.path.dirname(os.path.abspath(__file__))
        os.chdir(dirname)
        hostname = self.__getHostname()
        key = self.__matchKeyMap(hostname)
        f = self.__getKeyConfig(key)
        if f is None:
            print "bug"
        else:
            cmd = "%s -d -n %s -c %s" % (self.binary, self.filterUser, f)
            print "hostname:%s\nkey:%s\nredis config file:%s\ncmd:%s\n" % (hostname, key, f, cmd)
            os.system(cmd)

    def stop(self):
        cmd = "ps -eo pid,args | grep %s| grep -v grep|grep -o '[^ ]\+\( \+[^ ]\+\)*'|cut -d ' ' -f 1|xargs kill -9" % (self.binary)
        os.system(cmd)

    def status(self):
        """
            show session
            show drop percentage
        """
        print "status"
    def restart(self):
        self.stop()
        self.start()

    def help(self):
        print "Usage:"
        sys.stdout.write("\t python %s [" % (__file__))
        for i in dir(MysqlpcapConsole):
            # only show public functioin
            if i.find("_") != 0:
                sys.stdout.write(i)
                sys.stdout.write("|")
        #TODO
        print("]")

def main():
    s = MysqlpcapConsole()

    if len(sys.argv) == 1:
        s.help()
        sys.exit(1)

    # find para public function and execute it
    for i in dir(MysqlpcapConsole):
        if i.find("_") != 0:
            if i == sys.argv[1]:
                func = getattr(s, i)
                func()
                sys.exit(0)
    s.help()
    sys.exit(1)

if __name__ == "__main__":
    main()
