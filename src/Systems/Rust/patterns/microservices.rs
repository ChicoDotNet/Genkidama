pub fn run()->bool{let stock=|s:&str|s=="A";let price=|s:&str|if s=="A"{9}else{0};stock("A")&&price("A")==9}
