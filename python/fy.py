#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import json
import web
import random
import urllib2

URL = "http://fanyi.youdao.com/openapi.do?keyfrom=%s&key=%s&type=data&doctype=json&version=1.1&q=%s"
KEYFROM = "hoterrndict"
KEY = "354047444"

class YouDaoDict:
    """
        curl -X GET 
        "http://fanyi.youdao.com/openapi.do?keyfrom=hoterrndict&key=354047444&type=data&doctype=json&version=1.1&q=chrome"

    """

    def __init__(self, url=URL, keyfrom=KEYFROM, key=KEY):
        self.url = url
        self.keyfrom = keyfrom
        self.key = key

    def translate(self, query):
        url = self.url % (self.keyfrom, self.key, query)
        #request = urllib2.Request(url=url, data=json.dumps(msg))
        #request.add_header('Content-type', 'application/json')
        request = urllib2.Request(url=url)
        response = urllib2.urlopen(request)
        self.result = json.loads(response.read())
        print(self.result["basic"]["explains"][0])

    def save(self):
        pass

a = YouDaoDict()
a.translate(sys.argv[1])
