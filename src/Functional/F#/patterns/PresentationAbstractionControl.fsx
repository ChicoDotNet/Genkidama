module PresentationAbstractionControlExample
let run () =
    let view agentName value = $"{agentName}:view={value}"
    view "child" 42 = "child:view=42"
    && view "root" 42 = "root:view=42"
