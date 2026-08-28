class Handler:
    def __init__(self, name, limit, next_handler=None):
        self.name = name
        self.limit = limit
        self.next = next_handler

    def handle(self, amount, visited):
        visited.append(self.name)
        if amount <= self.limit:
            return self.name
        if self.next is None:
            return "none"
        return self.next.handle(amount, visited)


escalation = Handler("escalation", 10**9)
billing = Handler("billing", 500, escalation)
faq = Handler("faq", 50, billing)
visited = []
amount = 250
handled = faq.handle(amount, visited)
print("visited=%s;handled=%s;result=refund(%d)" % (">".join(visited), handled, amount))
