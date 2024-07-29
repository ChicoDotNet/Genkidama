interface Button {
    void render()
}

class DarkButton implements Button {
    void render() {
        println "Dark Button"
    }
}

class LightButton implements Button {
    void render() {
        println "Light Button"
    }
}

interface Checkbox {
    void render()
}

class DarkCheckbox implements Checkbox {
    void render() {
        println "Dark Checkbox"
    }
}

class LightCheckbox implements Checkbox {
    void render() {
        println "Light Checkbox"
    }
}

interface UIFactory {
    Button createButton()
    Checkbox createCheckbox()
}

class DarkFactory implements UIFactory {
    Button createButton() {
        return new DarkButton()
    }

    Checkbox createCheckbox() {
        return new DarkCheckbox()
    }
}

class LightFactory implements UIFactory {
    Button createButton() {
        return new LightButton()
    }

    Checkbox createCheckbox() {
        return new LightCheckbox()
    }
}

void createUIComponents(UIFactory factory) {
    Button button = factory.createButton()
    Checkbox checkbox = factory.createCheckbox()
    button.render()
    checkbox.render()
}

// Usage
createUIComponents(new DarkFactory())
createUIComponents(new LightFactory())
