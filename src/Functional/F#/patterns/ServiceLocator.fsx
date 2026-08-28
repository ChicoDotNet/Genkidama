module ServiceLocatorExample
let run () =
    let services =
        dict [
            "email", (fun value -> $"email>{value}")
            "audit", (fun value -> $"audit>{value}")
        ]
    services["email"] "a@example.test" = "email>a@example.test"
    && services["audit"] "created" = "audit>created"
