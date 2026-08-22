use std::sync::{Mutex, OnceLock};

static REGISTRY: OnceLock<Mutex<u32>> = OnceLock::new();

fn registry() -> &'static Mutex<u32> {
    REGISTRY.get_or_init(|| Mutex::new(0))
}

fn main() {
    let first = registry();
    let second = registry();
    *first.lock().expect("registry lock") += 1;
    println!("same={}", std::ptr::eq(first, second));
    println!("count={}", *second.lock().expect("registry lock"));
}
