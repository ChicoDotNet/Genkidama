module DependencyInjectionExample
let run () =
    let service clock = $"at:{clock ()}"
    service (fun () -> "10:00") = "at:10:00"
