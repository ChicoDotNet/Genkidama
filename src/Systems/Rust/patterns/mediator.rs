use std::collections::HashMap;

type Receiver = fn(&str, &str) -> String;

struct CheckoutMediator {
    colleagues: HashMap<&'static str, Receiver>,
}

impl CheckoutMediator {
    fn new() -> Self {
        Self {
            colleagues: HashMap::new(),
        }
    }

    fn register(&mut self, name: &'static str, receiver: Receiver) {
        self.colleagues.insert(name, receiver);
    }

    fn send(&self, sender: &str, recipient: &str, message: &str) -> Result<String, String> {
        self.colleagues
            .get(recipient)
            .map(|receiver| receiver(sender, message))
            .ok_or_else(|| format!("unknown colleague: {recipient}"))
    }
}

fn payment(sender: &str, message: &str) -> String {
    format!("payment<-{sender}:{message}")
}

fn inventory(sender: &str, message: &str) -> String {
    format!("inventory<-{sender}:{message}")
}

pub fn run() -> bool {
    let mut mediator = CheckoutMediator::new();
    mediator.register("payment", payment);
    mediator.register("inventory", inventory);

    let reserve = mediator.send("payment", "inventory", "reserve-order-42");
    let confirm = mediator.send("inventory", "payment", "reserved-order-42");
    let unknown = mediator.send("payment", "shipping", "dispatch-order-42");

    reserve.as_deref() == Ok("inventory<-payment:reserve-order-42")
        && confirm.as_deref() == Ok("payment<-inventory:reserved-order-42")
        && unknown.as_deref() == Err("unknown colleague: shipping")
}
