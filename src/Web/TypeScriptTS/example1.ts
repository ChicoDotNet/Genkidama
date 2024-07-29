// Abstract Product
interface Button {
    render(): void;
}

interface Checkbox {
    render(): void;
}

// Concrete Product
class DarkButton implements Button {
    render(): void {
        console.log("Dark Button");
    }
}

class LightButton implements Button {
    render(): void {
        console.log("Light Button");
    }
}

class DarkCheckbox implements Checkbox {
    render(): void {
        console.log("Dark Checkbox");
    }
}

class LightCheckbox implements Checkbox {
    render(): void {
        console.log("Light Checkbox");
    }
}

// Abstract Factory
interface UIFactory {
    createButton(): Button;
    createCheckbox(): Checkbox;
}

// Concrete Factory
class DarkFactory implements UIFactory {
    createButton(): Button {
        return new DarkButton();
    }
    
    createCheckbox(): Checkbox {
        return new DarkCheckbox();
    }
}

class LightFactory implements UIFactory {
    createButton(): Button {
        return new LightButton();
    }
    
    createCheckbox(): Checkbox {
        return new LightCheckbox();
    }
}

function createUIComponents(factory: UIFactory): void {
    const button = factory.createButton();
    const checkbox = factory.createCheckbox();
    button.render();
    checkbox.render();
}

// Usage
createUIComponents(new DarkFactory());
createUIComponents(new LightFactory());
