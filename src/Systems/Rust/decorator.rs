trait Component {
    fn render(&self) -> String;
}

struct PlainMessage;

impl Component for PlainMessage {
    fn render(&self) -> String {
        "alert".to_string()
    }
}

struct AuditDecorator {
    inner: Box<dyn Component>,
}

impl Component for AuditDecorator {
    fn render(&self) -> String {
        format!("audit({})", self.inner.render())
    }
}

struct EncryptDecorator {
    inner: Box<dyn Component>,
}

impl Component for EncryptDecorator {
    fn render(&self) -> String {
        format!("enc({})", self.inner.render())
    }
}

fn main() {
    let base = PlainMessage;
    println!("base={}", base.render());
    println!(
        "audit={}",
        AuditDecorator {
            inner: Box::new(PlainMessage),
        }
        .render()
    );
    println!(
        "encrypted={}",
        EncryptDecorator {
            inner: Box::new(PlainMessage),
        }
        .render()
    );
    println!(
        "stacked={}",
        AuditDecorator {
            inner: Box::new(EncryptDecorator {
                inner: Box::new(PlainMessage),
            }),
        }
        .render()
    );
}
