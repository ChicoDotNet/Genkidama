pub fn run()->bool{enum N{Num(i32),Text(&'static str)}fn visit(n:&N)->usize{match n{N::Num(v)=>*v as usize,N::Text(s)=>s.len()}}visit(&N::Num(3))+visit(&N::Text("ab"))==5}
