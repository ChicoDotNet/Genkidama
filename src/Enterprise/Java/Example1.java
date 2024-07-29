// Abstract Product
interface Button {
    void render();
}

interface Checkbox {
    void render();
}

// Concrete Product
class DarkButton implements Button {
    @Override
    public void render() {
        System.out.println("Dark Button");
    }
}

class LightButton implements Button {
    @Override
    public void render() {
        System.out.println("Light Button");
    }
}

class DarkCheckbox implements Checkbox {
    @Override
    public void render() {
        System.out.println("Dark Checkbox");
    }
}

class LightCheckbox implements Checkbox {
    @Override
    public void render() {
        System.out.println("Light Checkbox");
    }
}

// Abstract Factory
interface UIFactory {
    Button createButton();
    Checkbox createCheckbox();
}

// Concrete Factory
class DarkFactory implements UIFactory {
    @Override
    public Button createButton() {
        return new DarkButton();
    }
    
    @Override
    public Checkbox createCheckbox() {
        return new DarkCheckbox();
    }
}

class LightFactory implements UIFactory {
    @Override
    public Button createButton() {
        return new LightButton();
    }
    
    @Override
    public Checkbox createCheckbox() {
        return new LightCheckbox();
    }
}

public class Example1 {
    public static void createUIComponents(UIFactory factory) {
        Button button = factory.createButton();
        Checkbox checkbox = factory.createCheckbox();
        button.render();
        checkbox.render();
    }

    public static void main(String[] args) {
        createUIComponents(new DarkFactory());
        createUIComponents(new LightFactory());
    }
}
