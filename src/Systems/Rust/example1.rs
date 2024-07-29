trait Button {
    fn render(&self);
}

struct DarkButton;

impl Button for DarkButton {
    fn render(&self) {
        println!("Dark Button");
    }
}

struct LightButton;

impl Button for LightButton {
    fn render(&self) {
        println!("Light Button");
    }
}

trait Checkbox {
    fn render(&self);
}

struct DarkCheckbox;

impl Checkbox for DarkCheckbox {
    fn render(&self) {
        println!("Dark Checkbox");
    }
}

struct LightCheckbox;

impl Checkbox for LightCheckbox {
    fn render(&self) {
        println!("Light Checkbox");
    }
}

trait UIFactory {
    fn create_button(&self) -> Box<dyn Button>;
    fn create_checkbox(&self) -> Box<dyn Checkbox>;
}

struct DarkFactory;

impl UIFactory for DarkFactory {
    fn create_button(&self) -> Box<dyn Button> {
        Box::new(DarkButton)
    }
    fn create_checkbox(&self) -> Box<dyn Checkbox> {
        Box::new(DarkCheckbox)
    }
}

struct LightFactory;

impl UIFactory for LightFactory {
    fn create_button(&self) -> Box<dyn Button> {
        Box::new(LightButton)
    }
    fn create_checkbox(&self) -> Box<dyn Checkbox> {
        Box::new(LightCheckbox)
    }
}

fn create_ui_components(factory: &dyn UIFactory) {
    let button = factory.create_button();
    let checkbox = factory.create_checkbox();
    button.render();
    checkbox.render();
}

fn main() {
    let dark_factory = DarkFactory;
    let light_factory = LightFactory;
    create_ui_components(&dark_factory);
    create_ui_components(&light_factory);
}
