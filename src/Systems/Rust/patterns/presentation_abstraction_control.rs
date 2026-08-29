pub fn run()->bool{let a=3;let control=|x|x+1;let presentation=|x|format!("n={x}");presentation(control(a))=="n=4"}
