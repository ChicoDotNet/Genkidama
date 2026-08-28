pub fn run()->bool{let stock=|_:&str|true;let pay=|n|n==9;let facade=|s,n|stock(s)&&pay(n);facade("A",9)}
