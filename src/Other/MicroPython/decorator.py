class PlainMessage:
    def render(self):
        return "alert"


class ComponentDecorator:
    def __init__(self, inner):
        self.inner = inner

    def render(self):
        return self.inner.render()


class AuditDecorator(ComponentDecorator):
    def render(self):
        return "audit(" + self.inner.render() + ")"


class EncryptDecorator(ComponentDecorator):
    def render(self):
        return "enc(" + self.inner.render() + ")"


base = PlainMessage()
audited = AuditDecorator(base)
encrypted = EncryptDecorator(base)
stacked = AuditDecorator(EncryptDecorator(base))

print("base=" + base.render())
print("audit=" + audited.render())
print("encrypted=" + encrypted.render())
print("stacked=" + stacked.render())
