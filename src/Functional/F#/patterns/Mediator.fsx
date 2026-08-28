module MediatorExample
open System.Collections.Generic
let run ()=let events=ResizeArray<string>() in events.Add("panel.refresh");events.Add("button.enable");System.String.concat ">" events="panel.refresh>button.enable"
