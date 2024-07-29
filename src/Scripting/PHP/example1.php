<?php

interface Button {
    public function render();
}

class DarkButton implements Button {
    public function render() {
        echo "Dark Button\n";
    }
}

class LightButton implements Button {
    public function render() {
        echo "Light Button\n";
    }
}

interface Checkbox {
    public function render();
}

class DarkCheckbox implements Checkbox {
    public function render() {
        echo "Dark Checkbox\n";
    }
}

class LightCheckbox implements Checkbox {
    public function render() {
        echo "Light Checkbox\n";
    }
}

interface UIFactory {
    public function createButton(): Button;
    public function createCheckbox(): Checkbox;
}

class DarkFactory implements UIFactory {
    public function createButton(): Button {
        return new DarkButton();
    }

    public function createCheckbox(): Checkbox {
        return new DarkCheckbox();
    }
}

class LightFactory implements UIFactory {
    public function createButton(): Button {
        return new LightButton();
    }

    public function createCheckbox(): Checkbox {
        return new LightCheckbox();
    }
}

function createUIComponents(UIFactory $factory) {
    $button = $factory->createButton();
    $checkbox = $factory->createCheckbox();
    $button->render();
    $checkbox->render();
}

// Usage
createUIComponents(new DarkFactory());
createUIComponents(new LightFactory());
