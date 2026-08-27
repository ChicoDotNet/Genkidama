struct Handler {
    name: &'static str,
    max_amount: Option<i32>,
    next: Option<Box<Handler>>,
}

impl Handler {
    fn new(name: &'static str, max_amount: Option<i32>, next: Option<Box<Handler>>) -> Self {
        Self {
            name,
            max_amount,
            next,
        }
    }

    fn handle<'a>(&'a self, amount: i32, visited: &mut Vec<&'a str>) -> &'a str {
        visited.push(self.name);
        if self.max_amount.is_none_or(|max| amount <= max) {
            return self.name;
        }

        self.next
            .as_deref()
            .expect("No handler accepted the request.")
            .handle(amount, visited)
    }
}

fn main() {
    let chain = Handler::new(
        "faq",
        Some(50),
        Some(Box::new(Handler::new(
            "billing",
            Some(500),
            Some(Box::new(Handler::new("escalation", None, None))),
        ))),
    );

    let mut visited = Vec::new();
    let handled = chain.handle(250, &mut visited);
    println!(
        "visited={};handled={};result=refund(250)",
        visited.join(">"),
        handled
    );
}
