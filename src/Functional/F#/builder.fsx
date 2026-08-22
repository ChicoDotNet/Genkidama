type ReportBuilder = {
    Reset: unit -> unit
    AddTitle: string -> unit
    AddSection: string -> string -> unit
    Build: unit -> string
}

let textBuilder () =
    let parts = System.Collections.Generic.List<string>()
    {
        Reset = fun () -> parts.Clear()
        AddTitle = fun title -> parts.Add($"# {title}")
        AddSection = fun heading body ->
            parts.Add($"## {heading}")
            parts.Add(body)
        Build = fun () -> System.String.Join("\n", parts)
    }

let htmlBuilder () =
    let parts = System.Collections.Generic.List<string>()
    {
        Reset = fun () -> parts.Clear()
        AddTitle = fun title -> parts.Add($"<h1>{title}</h1>")
        AddSection = fun heading body ->
            parts.Add($"<h2>{heading}</h2>")
            parts.Add($"<p>{body}</p>")
        Build = fun () -> System.String.Concat(parts)
    }

let buildAvailabilityReport (builder: ReportBuilder) =
    builder.Reset()
    builder.AddTitle "Service status"
    builder.AddSection "Availability" "99.95%"
    builder.Build()

printfn "%s" (buildAvailabilityReport (textBuilder()))
printfn "---"
printfn "%s" (buildAvailabilityReport (htmlBuilder()))
