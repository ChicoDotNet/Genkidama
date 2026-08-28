pub fn run()->bool{let d="abc";let v1=|s:&str|s.len();let v2=|s:&str|s.bytes().count();v1(d)==3&&v2(d)==3}
