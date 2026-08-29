pub fn run()->bool{trait Log{fn write(&self,_:&str)->usize;}struct Null;impl Log for Null{fn write(&self,_:&str)->usize{0}}Null.write("ignored")==0}
