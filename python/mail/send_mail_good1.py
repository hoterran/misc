#!/usr/bin/env python
#coding: utf-8

import smtplib

mailserver="x.com"
from_addr="x@alibaba-inc.com"
to_addr="x@alibaba-inc.com"

msg="ABTN"

svr=smtplib.SMTP(mailserver, 25)

svr.set_debuglevel(1)

svr.sendmail(from_addr, to_addr, msg)

svr.quit()

