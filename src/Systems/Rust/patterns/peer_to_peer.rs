pub fn run()->bool{let a=|m:&str|format!("a:{m}");let b=|m:&str|format!("b:{m}");b(&a("hi"))=="b:a:hi"}
