// Abstract Factory
function createButton(theme) {
    if (theme === 'dark') {
        return darkButton;
    } else if (theme === 'light') {
        return lightButton;
    }
}

function createCheckbox(theme) {
    if (theme === 'dark') {
        return darkCheckbox;
    } else if (theme === 'light') {
        return lightCheckbox;
    }
}

// Concrete Products
function darkButton() {
    console.log("Dark Button");
}

function lightButton() {
    console.log("Light Button");
}

function darkCheckbox() {
    console.log("Dark Checkbox");
}

function lightCheckbox() {
    console.log("Light Checkbox");
}

// Usage
const button = createButton('dark');
button();

const checkbox = createCheckbox('light');
checkbox();
