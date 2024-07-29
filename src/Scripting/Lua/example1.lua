-- Abstract Factory
UIFactory = {}
function UIFactory:createButton() end
function UIFactory:createCheckbox() end

-- Concrete Factory
DarkFactory = UIFactory:new()
function DarkFactory:createButton()
    return DarkButton:new()
end
function DarkFactory:createCheckbox()
    return DarkCheckbox:new()
end

LightFactory = UIFactory:new()
function LightFactory:createButton()
    return LightButton:new()
end
function LightFactory:createCheckbox()
    return LightCheckbox:new()
end

-- Products
Button = {}
function Button:render() end

DarkButton = Button:new()
function DarkButton:render()
    print("Dark Button")
end

LightButton = Button:new()
function LightButton:render()
    print("Light Button")
end

Checkbox = {}
function Checkbox:render() end

DarkCheckbox = Checkbox:new()
function DarkCheckbox:render()
    print("Dark Checkbox")
end

LightCheckbox = Checkbox:new()
function LightCheckbox:render()
    print("Light Checkbox")
end

function createUIComponents(factory)
    local button = factory:createButton()
    local checkbox = factory:createCheckbox()
    button:render()
    checkbox:render()
end

-- Usage
createUIComponents(DarkFactory)
createUIComponents(LightFactory)
