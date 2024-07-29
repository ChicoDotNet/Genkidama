// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Abstract Product
abstract contract Button {
    function render() public virtual view returns (string memory);
}

abstract contract Checkbox {
    function render() public virtual view returns (string memory);
}

// Concrete Product
contract DarkButton is Button {
    function render() public pure override returns (string memory) {
        return "Dark Button";
    }
}

contract LightButton is Button {
    function render() public pure override returns (string memory) {
        return "Light Button";
    }
}

contract DarkCheckbox is Checkbox {
    function render() public pure override returns (string memory) {
        return "Dark Checkbox";
    }
}

contract LightCheckbox is Checkbox {
    function render() public pure override returns (string memory) {
        return "Light Checkbox";
    }
}

// Abstract Factory
abstract contract UIFactory {
    function createButton() public virtual returns (Button);
    function createCheckbox() public virtual returns (Checkbox);
}

// Concrete Factory
contract DarkFactory is UIFactory {
    function createButton() public override returns (Button) {
        return new DarkButton();
    }
    
    function createCheckbox() public override returns (Checkbox) {
        return new DarkCheckbox();
    }
}

contract LightFactory is UIFactory {
    function createButton() public override returns (Button) {
        return new LightButton();
    }
    
    function createCheckbox() public override returns (Checkbox) {
        return new LightCheckbox();
    }
}

contract Example1 {
    function createUIComponents(UIFactory factory) public view returns (string memory, string memory) {
        Button button = factory.createButton();
        Checkbox checkbox = factory.createCheckbox();
        return (button.render(), checkbox.render());
    }

    function test() public view returns (string memory, string memory, string memory, string memory) {
        UIFactory darkFactory = new DarkFactory();
        UIFactory lightFactory = new LightFactory();
        (string memory darkButton, string memory darkCheckbox) = createUIComponents(darkFactory);
        (string memory lightButton, string memory lightCheckbox) = createUIComponents(lightFactory);
        return (darkButton, darkCheckbox, lightButton, lightCheckbox);
    }
}
