import smtplib
from email.MIMEText import MIMEText
from email.MIMEMultipart import MIMEMultipart

smtp_sever = smtplib.SMTP('smtp.ops.aliyun-inc.com')

msgRoot = MIMEMultipart('related')
msg['From'] = "ruoyi.ruanry@alibaba-inc.com"
msg['To'] = "ruoyi.ruanry@alibaba-inc.com"
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

