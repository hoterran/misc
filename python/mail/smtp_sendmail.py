import smtplib
from email.MIMEText import MIMEText
from email.MIMEMultipart import MIMEMultipart

smtp_sever = smtplib.SMTP('x.com')

msgRoot = MIMEMultipart('related')
msg['From'] = "x@x.com"
msg['To'] = "x@x.com"
msg['Subject'] = 'Hello1111'

msgAlernative = MIMEMultipart('alernative')
msgRoot.attach(msgAlernative)

html="<html><body>Hello world</body></html>"
msgHtml = MIMEText(html, 'html')
msgAlernative.attach(msgHtml)

text = 'Hello world'
msgTxt = MIMEText(text)
msgAlernative.attach(msgTxt)

smtp_server.sendmail(sender, recepient, msgRoot.as_string())

