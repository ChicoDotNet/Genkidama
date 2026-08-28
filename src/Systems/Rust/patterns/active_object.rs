pub fn run()->bool{let q:Vec<Box<dyn Fn()->i32>>=vec![Box::new(||2),Box::new(||3)];q.into_iter().map(|f|f()).sum::<i32>()==5}
