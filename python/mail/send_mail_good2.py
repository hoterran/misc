#!/usr/bin/env python
#coding: utf-8
import smtplib
import time

# Import the email modules we'll need
from email.mime.text import MIMEText

# Open a plain text file for reading.  For this example, assume that
# the text file contains only ASCII characters.
fp = open("funk", 'rb')
# Create a text/plain message
msg = MIMEText(fp.read())
fp.close()

me = "x@x.com"
you = "x@x.com, x@163.com"

mon=time.localtime().tm_mon

msg['Subject'] = 'ABTN btc %d 月流量' % mon
msg['From'] = me
msg['To'] = you

# Send the message via our own SMTP server, but don't include the
# envelope header.
s = smtplib.SMTP("x.com")
s.sendmail(me, you, msg.as_string())
s.quit()
