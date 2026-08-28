final class Handler {
    final String name
    final int limit
    Handler next

    Handler(String name, int limit) {
        this.name = name
        this.limit = limit
    }

    String handle(int amount, List<String> visited) {
        visited << name
        if (amount <= limit || next == null) {
            return "visited=${visited.join('>')};handled=${name};result=refund(${amount})"
        }
        next.handle(amount, visited)
    }
}

def faq = new Handler('faq', 50)
def billing = new Handler('billing', 500)
def escalation = new Handler('escalation', Integer.MAX_VALUE)
faq.next = billing
billing.next = escalation
println faq.handle(250, [])
