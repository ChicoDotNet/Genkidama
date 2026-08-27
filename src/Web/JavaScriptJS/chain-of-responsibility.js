class RefundHandler {
  constructor(name, canHandle) {
    this.name = name;
    this.canHandle = canHandle;
    this.next = null;
  }

  setNext(next) {
    this.next = next;
    return next;
  }

  handle(amount, visited) {
    visited.push(this.name);
    if (this.canHandle(amount)) {
      return this.name;
    }
    if (!this.next) {
      throw new Error("No handler accepted the request.");
    }
    return this.next.handle(amount, visited);
  }
}

const faq = new RefundHandler("faq", (amount) => amount <= 50);
const billing = new RefundHandler("billing", (amount) => amount <= 500);
const escalation = new RefundHandler("escalation", () => true);
faq.setNext(billing).setNext(escalation);

const visited = [];
const handled = faq.handle(250, visited);
console.log(`visited=${visited.join(">")};handled=${handled};result=refund(250)`);
