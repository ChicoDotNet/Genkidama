pub fn run()->bool{let server=|r:&str|format!("echo:{r}");let client=|v|server(v);client("ping")=="echo:ping"}
