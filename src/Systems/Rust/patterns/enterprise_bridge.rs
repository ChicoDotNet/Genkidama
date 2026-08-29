pub fn run()->bool{let imp=|s:&str|format!("[{s}]");let abstraction=|s|imp(s);abstraction("x")=="[x]"}
