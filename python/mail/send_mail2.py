#!/usr/bin/env python
#coding: utf-8
import smtplib
import time
import os.path

# Import the email modules we'll need
import email.MIMEText
import email.MIMEMultipart
import email.MIMEBase 
import email.Encoders

context="context.txt"
attachment="attachement.txt"

# Open a plain text file for reading.  For this example, assume that
# the text file contains only ASCII characters.
# Create a text/plain message
msg = email.MIMEMultipart.MIMEMultipart()

#context
msg.attach(email.MIMEText.MIMEText(file(context).read()))
#attachement
contype = 'application/octet-stream'
maintype, subtype = contype.split('/', 1)

data = open(attachment, 'rb')
file_msg = email.MIMEBase.MIMEBase(maintype, subtype)
file_msg.set_payload(data.read( ))
data.close( ) 
email.Encoders.encode_base64(file_msg) 

basename = os.path.basename(attachment)
file_msg.add_header('Content-Disposition', 
 'attachment', filename = basename)
msg.attach(file_msg)

me = "ruoyi.ruanry@alibaba-inc.com"
you = "ruoyi.ruanry@alibaba-inc.com, hoterran@163.com"

# Send the message via our own SMTP server, but don't include the
# envelope header.
s = smtplib.SMTP("x.com")
s.sendmail(me, you, msg.as_string())
s.quit()
