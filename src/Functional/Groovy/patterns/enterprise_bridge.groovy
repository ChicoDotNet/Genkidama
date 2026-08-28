def sender={"sms:${it}"};def notify={sender(it)};assert notify('ok')=='sms:ok'
