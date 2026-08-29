pub fn run()->bool{let s:[fn(i32)->i32;2]=[|x|x+1,|x|x+2];s[0](5)==6&&s[1](5)==7}
