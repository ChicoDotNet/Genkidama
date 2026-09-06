type Receiver = (sender: string, message: string) => void;

class CheckoutMediator {
  private readonly colleagues = new Map<string, Receiver>();

  register(name: string, receiver: Receiver): void {
    this.colleagues.set(name, receiver);
  }

  send(sender: string, recipient: string, message: string): void {
    const receiver = this.colleagues.get(recipient);
    if (!receiver) {
      throw new Error(`unknown colleague: ${recipient}`);
    }
    receiver(sender, message);
  }
}

function mediatorPattern(): boolean {
  const mediator = new CheckoutMediator();
  const received: string[] = [];

  mediator.register('payment', (sender, message) => received.push(`payment<-${sender}:${message}`));
  mediator.register('inventory', (sender, message) => received.push(`inventory<-${sender}:${message}`));

  mediator.send('payment', 'inventory', 'reserve');
  mediator.send('inventory', 'payment', 'reserved');

  let rejectedUnknown = false;
  try {
    mediator.send('payment', 'shipping', 'dispatch');
  } catch (error) {
    rejectedUnknown = error instanceof Error && error.message === 'unknown colleague: shipping';
  }

  return received.join('>') === 'inventory<-payment:reserve>payment<-inventory:reserved'
    && rejectedUnknown;
}

if (!mediatorPattern()) {
  throw new Error('Mediator pattern verification failed');
}
