pub fn run()->bool{fn flow(h:fn(&str)->String)->String{format!("start>{}>end",h("work"))}flow(|s|s.to_uppercase())=="start>WORK>end"}
