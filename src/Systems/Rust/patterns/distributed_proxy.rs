pub fn run()->bool{let remote=|id|format!("item:{id}");let proxy=|id|remote(id);proxy(7)=="item:7"}
