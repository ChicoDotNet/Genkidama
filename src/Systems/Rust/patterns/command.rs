pub fn run()->bool{let mut b=100;let ops:[fn(i32)->i32;2]=[|x|x+50,|x|x-20];for op in ops{b=op(b);}b==130}
