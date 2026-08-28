pub fn run()->bool{let p:[fn(i32)->i32;2]=[|x|x+1,|x|x*2];p.iter().fold(3,|v,f|f(v))==8}
