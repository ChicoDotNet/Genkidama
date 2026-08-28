pub fn run()->bool{let mut h=std::collections::HashMap::new();h.insert("price",9);let broker=|t|h.get(t).copied().unwrap_or(0);broker("price")==9}
