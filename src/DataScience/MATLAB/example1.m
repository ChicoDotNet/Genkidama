function example1
% Abstract Factory represented as a struct of related constructors.
% Select the family once, then create all related products from it.

darkFactory = createFactory(@darkButton, @darkCheckbox);
lightFactory = createFactory(@lightButton, @lightCheckbox);

createUIComponents(darkFactory);
createUIComponents(lightFactory);
end

function factory = createFactory(buttonConstructor, checkboxConstructor)
factory = struct( ...
    'createButton', buttonConstructor, ...
    'createCheckbox', checkboxConstructor);
end

function createUIComponents(factory)
buttonConstructor = factory.createButton;
checkboxConstructor = factory.createCheckbox;
buttonConstructor();
checkboxConstructor();
end

function darkButton
fprintf('Dark Button\n');
end

function lightButton
fprintf('Light Button\n');
end

function darkCheckbox
fprintf('Dark Checkbox\n');
end

function lightCheckbox
fprintf('Light Checkbox\n');
end
