% Abstract Factory
function button = createButton(theme)
    if strcmp(theme, 'dark')
        button = @darkButton;
    elseif strcmp(theme, 'light')
        button = @lightButton;
    end
end

function checkbox = createCheckbox(theme)
    if strcmp(theme, 'dark')
        checkbox = @darkCheckbox;
    elseif strcmp(theme, 'light')
        checkbox = @lightCheckbox;
    end
end

% Concrete Products
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

% Usage
button = createButton('dark');
button();

checkbox = createCheckbox('light');
checkbox();
