#!/usr/bin/env python
#coding: utf-8

import smtplib

mailserver="smtp.ops.aliyun-inc.com"
from_addr="ruoyi.ruanry@alibaba-inc.com"
to_addr="ruoyi.ruanry@alibaba-inc.com"

msg="ABTN"

svr=smtplib.SMTP(mailserver, 25)

svr.set_debuglevel(1)

svr.sendmail(from_addr, to_addr, msg)

svr.quit()

