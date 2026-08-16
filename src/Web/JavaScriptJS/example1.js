// Concrete products
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

// Concrete factories. Selecting one object selects the complete product family.
const darkFactory = Object.freeze({
    createButton: () => darkButton,
    createCheckbox: () => darkCheckbox,
});

const lightFactory = Object.freeze({
    createButton: () => lightButton,
    createCheckbox: () => lightCheckbox,
});

/**
 * Resolves one coherent UI family.
 * @param {"dark" | "light"} theme Requested UI family.
 * @returns {{createButton: () => Function, createCheckbox: () => Function}} Factory for that family.
 * @throws {RangeError} When the theme is not supported.
 */
function createUIFactory(theme) {
    if (theme === "dark") {
        return darkFactory;
    }

    if (theme === "light") {
        return lightFactory;
    }

    throw new RangeError(`Unsupported theme: ${theme}`);
}

/**
 * Creates and renders both products from the same selected family.
 * @param {{createButton: () => Function, createCheckbox: () => Function}} factory Selected family factory.
 */
function createUIComponents(factory) {
    const button = factory.createButton();
    const checkbox = factory.createCheckbox();
    button();
    checkbox();
}

createUIComponents(createUIFactory("dark"));
createUIComponents(createUIFactory("light"));
