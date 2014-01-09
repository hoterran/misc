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

def connect(server, port):
	if not server:
		return -1

	if not port:
		port=25

#	print server
	s = smtplib.SMTP(server, port)
#	s.set_debuglevel(1)
	return s

def close(s):
	if not s:
		return -1
	s.quit()
	return 0

def add_context(fr, to, subject, context_file):
	if not (fr or to or subject or context_file):
		return -1
	
	msg = email.MIMEMultipart.MIMEMultipart()
	msg.attach(email.MIMEText.MIMEText(file(context_file).read()))

	msg['Subject'] = subject
	msg['From'] = fr 
	msg['to'] = to	
	return msg

def add_attachment(msg, attachment):
	if not (msg or attachment):
		return -1

	contype = 'application/octet-stream'
	maintype, subtype = contype.split('/', 1)

	data = open(attachment, 'rb')

	if None == data:
		return -1

	file_msg = email.MIMEBase.MIMEBase(maintype, subtype)
	file_msg.set_payload(data.read( ))
	data.close( ) 
	email.Encoders.encode_base64(file_msg) 

	basename = os.path.basename(attachment)
	file_msg.add_header('Content-Disposition', 'attachment', filename = basename)
	msg.attach(file_msg)

	return 0

def send(s, msg):
	if not msg:
		return -1

	s.sendmail(msg['From'], msg['To'].split(","), msg.as_string())

	return 0
