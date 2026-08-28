pub fn run()->bool{let model=4;let controller=|x|x+1;let view=|x|format!("value={x}");view(controller(model))=="value=5"}
