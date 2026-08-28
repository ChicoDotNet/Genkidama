module StrategyExample
let run () =
    let price value strategy = strategy value
    price 100 id = 100
    && price 100 (fun value -> value * 80 / 100) = 80
