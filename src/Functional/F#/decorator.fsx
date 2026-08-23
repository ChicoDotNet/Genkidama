type Component = { render: unit -> string }

let plainMessage = { render = fun () -> "alert" }

let auditDecorator (inner: Component) =
    { render = fun () -> $"audit({inner.render()})" }

let encryptDecorator (inner: Component) =
    { render = fun () -> $"enc({inner.render()})" }

printfn "base=%s" (plainMessage.render())
printfn "audit=%s" ((auditDecorator plainMessage).render())
printfn "encrypted=%s" ((encryptDecorator plainMessage).render())
printfn "stacked=%s" ((auditDecorator (encryptDecorator plainMessage)).render())
