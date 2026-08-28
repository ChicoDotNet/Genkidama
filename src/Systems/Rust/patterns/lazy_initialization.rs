pub fn run()->bool{let mut c=None;let v=*c.get_or_insert_with(||7);v==7&&c==Some(7)}
