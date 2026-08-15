% Abstract Factory represented as a struct of related constructors.
% Select the family once, then create all related products from it.

darkFactory = createFactory(@darkButton, @darkCheckbox);
lightFactory = createFactory(@lightButton, @lightCheckbox);

createUIComponents(darkFactory);
createUIComponents(lightFactory);

function factory = createFactory(buttonConstructor, checkboxConstructor)
    factory = struct( ...
        'createButton', buttonConstructor, ...
        'createCheckbox', checkboxConstructor);
end

function createUIComponents(factory)
    factory.createButton();
    factory.createCheckbox();
end

function darkButton()
    disp('Dark Button');
end

function lightButton()
    disp('Light Button');
end

function darkCheckbox()
    disp('Dark Checkbox');
end

function lightCheckbox()
    disp('Light Checkbox');
end
