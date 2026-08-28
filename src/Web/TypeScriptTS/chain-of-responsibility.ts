interface RefundHandler {
  setNext(next: RefundHandler): RefundHandler;
  handle(amount: number, visited: string[]): string;
}

abstract class BaseHandler implements RefundHandler {
  private next?: RefundHandler;

  protected abstract readonly name: string;
  protected abstract canHandle(amount: number): boolean;

  setNext(next: RefundHandler): RefundHandler {
    this.next = next;
    return next;
  }

  handle(amount: number, visited: string[]): string {
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

class FaqHandler extends BaseHandler {
  protected readonly name = "faq";
  protected canHandle(amount: number): boolean {
    return amount <= 50;
  }
}

class BillingHandler extends BaseHandler {
  protected readonly name = "billing";
  protected canHandle(amount: number): boolean {
    return amount <= 500;
  }
}

class EscalationHandler extends BaseHandler {
  protected readonly name = "escalation";
  protected canHandle(_amount: number): boolean {
    return true;
  }
}

const faq = new FaqHandler();
const billing = new BillingHandler();
const escalation = new EscalationHandler();
faq.setNext(billing).setNext(escalation);

const visited: string[] = [];
const handled = faq.handle(250, visited);
console.log(`visited=${visited.join(">")}\u003bhandled=${handled}\u003bresult=refund(250)`);
