use std::{collections::HashMap, rc::Rc};
#[derive(Debug)] struct TextStyle { font: String, size: u32, color: String }
struct StyleFactory { styles: HashMap<String, Rc<TextStyle>> }
impl StyleFactory {
    fn new() -> Self { Self { styles: HashMap::new() } }
    fn get(&mut self, font:&str,size:u32,color:&str)->Rc<TextStyle>{
        let key=format!("{font}|{size}|{color}");
        self.styles.entry(key).or_insert_with(||Rc::new(TextStyle{font:font.into(),size,color:color.into()})).clone()
    }
    fn count(&self)->usize{self.styles.len()}
}
fn main(){let mut f=StyleFactory::new();let r1=f.get("Inter",12,"red");let r2=f.get("Inter",12,"red");let _b=f.get("Inter",12,"blue");let _=&(r1.font.as_str(),r1.size,r1.color.as_str());println!("styles={};shared={};text=ABC",f.count(),Rc::ptr_eq(&r1,&r2));}
