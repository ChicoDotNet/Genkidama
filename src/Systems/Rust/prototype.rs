#[derive(Clone)]
struct ServiceProfile {
    name: String,
    features: Vec<String>,
}

impl ServiceProfile {
    fn describe(&self) -> String {
        format!("{}: {}", self.name, self.features.join(","))
    }
}

fn main() {
    let original = ServiceProfile {
        name: "orders".to_owned(),
        features: vec!["metrics".to_owned()],
    };
    let mut canary = original.clone();

    canary.name = "orders-canary".to_owned();
    canary.features.push("tracing".to_owned());

    println!("original={}", original.describe());
    println!("clone={}", canary.describe());
}
