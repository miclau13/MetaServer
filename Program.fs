// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu

type CliError =
    | ArgumentsNotSpecified

type CmdArgs =
    | [<AltCommandLine("-p")>] Print of message:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Print _ -> "Print a message"

let getExitCode result =
    match result with
    | Ok () -> 0
    | Error err ->
        match err with
        | ArgumentsNotSpecified -> 1

let runPrint print = 
    printfn "%s" print
    Ok ()

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CmdArgs>(programName = "metaserver", errorHandler = errorHandler)
    
    match parser.ParseCommandLine argv with
    | p when p.Contains(Print) -> runPrint (p.GetResult(Print))
    | _ ->
        printfn "%s" (parser.PrintUsage())
        Error ArgumentsNotSpecified
    |> getExitCode