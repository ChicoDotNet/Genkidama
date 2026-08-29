pub fn run()->bool{let n=std::sync::Mutex::new(0);*n.lock().unwrap()+=1;*n.lock().unwrap()==1}
