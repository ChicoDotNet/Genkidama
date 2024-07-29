type IButton =
    abstract member Render: unit -> unit

type DarkButton() =
    interface IButton with
        member _.Render() = printfn "Dark Button"

type LightButton() =
    interface IButton with
        member _.Render() = printfn "Light Button"

type ICheckbox =
    abstract member Render: unit -> unit

type DarkCheckbox() =
    interface ICheckbox with
        member _.Render() = printfn "Dark Checkbox"

type LightCheckbox() =
    interface ICheckbox with
        member _.Render() = printfn "Light Checkbox"

type UIFactory =
    abstract member CreateButton: unit -> IButton
    abstract member CreateCheckbox: unit -> ICheckbox

type DarkFactory() =
    interface UIFactory with
        member _.CreateButton() = DarkButton() :> IButton
        member _.CreateCheckbox() = DarkCheckbox() :> ICheckbox

type LightFactory() =
    interface UIFactory with
        member _.CreateButton() = LightButton() :> IButton
        member _.CreateCheckbox() = LightCheckbox() :> ICheckbox

let createUIComponents (factory: UIFactory) =
    let button = factory.CreateButton()
    let checkbox = factory.CreateCheckbox()
    button.Render()
    checkbox.Render()

// Usage
createUIComponents (DarkFactory() :> UIFactory)
createUIComponents (LightFactory() :> UIFactory)
