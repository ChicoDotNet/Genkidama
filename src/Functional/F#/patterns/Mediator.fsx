module MediatorExample
open System.Collections.Generic

let run () =
    let events = ResizeArray<string>()
    let notify sender event =
        match sender, event with
        | "button", "click" -> events.Add("panel.refresh")
        | "panel", "loaded" -> events.Add("button.enable")
        | _ -> ()
    notify "button" "click"
    notify "panel" "loaded"
    String.concat ">" events = "panel.refresh>button.enable"
