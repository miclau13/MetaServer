// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu

type CliError =
    | ArgumentsNotSpecified

type CmdArgs =
    | [<AltCommandLine("-p")>] Print of message:string
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Print _ -> "Print a message"
            | List _ -> "List file(s)"
and ListArgs =
    | [<AltCommandLine("-a")>] All
    | [<AltCommandLine("-s")>] Start of msg:string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "List all Grids."
            | Start _ -> "Use the given <start> as the starting date. "

let runList (runArgs: ParseResults<ListArgs>) =
    match runArgs with
    | argz when argz.Contains(All) ->
        // let allArgs = runArgs.GetResult(All)
        // List all grid using Neo4j
        // printfn "%A" "All Grids..."
        Neo4j.createIfNotExist()

        // let result = Neo4j.createMultipleFiles()
        // Neo4j.relateFile()
        // Neo4j.deleteAllFiles
        let result = Neo4j.getAllFile()
        // let result = "Done"
        printfn "Result: %A " result
        Ok ()
    | argz when argz.Contains(Start) ->
        // Get the start date 
        let startArgs = runArgs.GetResult(Start)
        // List all grid using Neo4j
        printfn "%A %A" "startArgs:" startArgs
        Ok ()
    | _ -> 
        printfn "%s" "No argument provided"
        Error ArgumentsNotSpecified

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
    | p when p.Contains(List) -> runList (p.GetResult(List))
    | _ ->
        printfn "%s" (parser.PrintUsage())
        Error ArgumentsNotSpecified
    |> getExitCode