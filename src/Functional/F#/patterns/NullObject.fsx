module NullObjectExample

type ILogger =
    abstract member Log: string -> string

type NullLogger() =
    interface ILogger with
        member _.Log _ = ""

type RealLogger() =
    interface ILogger with
        member _.Log message = $"log:{message}"

let run () =
    let nullLogger = NullLogger() :> ILogger
    let realLogger = RealLogger() :> ILogger
    nullLogger.Log("x") = "" && realLogger.Log("x") = "log:x"
