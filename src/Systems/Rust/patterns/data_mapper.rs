pub fn run()->bool{let row=(1,"Ada");let map=|(id,name):(i32,&str)|(id,name.to_string());let d=map(row);d.0==1&&d.1=="Ada"}
