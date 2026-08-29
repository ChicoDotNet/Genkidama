pub fn run()->bool{let o:[fn(i32)->i32;2]=[|x|x+1,|x|x*2];o[0](3)==4&&o[1](3)==6}
