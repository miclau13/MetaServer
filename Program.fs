// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu

type CliError =
    | ArgumentsNotSpecified

type CmdArgs =
    | [<AltCommandLine("-p")>] Print of message:string
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>

with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Print _ -> "Print a message"
            | List _ -> "List node(s)"
            | Init _ -> "Init nodes and relationships"
and InitArgs =
    | [<AltCommandLine("-a")>] All
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Init all."

and ListArgs =
    | [<AltCommandLine("-a")>] All
    | [<AltCommandLine("-s")>] Start of msg:string
    | [<AltCommandLine("-l")>] Label of msg:string
    | [<AltCommandLine("-r")>] Relationship of msg:string option
    | [<AltCommandLine("-c")>] Checksum of msg:string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "List all Grids."
            | Start _ -> "Use the given <start> as the starting date."
            | Label _ -> "Find the corresping nodes with the <label>."
            | Relationship _ -> "Find the Relationship of the node."
            | Checksum _ -> "List the node by <checksum>."

let runInit (runArgs: ParseResults<InitArgs>) =
    match runArgs with
    | _ ->
        Neo4j.deleteAllNodes()
        let result = Neo4j.createInitNodesIfNotExist ()
        printfn "Result: %A " result
        Neo4j.relateInitNodes ()
        Ok ()

let runList (runArgs: ParseResults<ListArgs>) =
    match runArgs with
    | argz when argz.Contains(All) ->
        let result = Neo4j.getAllNodes ()
        for i in result do
            printfn "%s" i
        Ok ()
    | argz when argz.Contains(Start) ->
        // Get the start date 
        let startArgs = runArgs.GetResult(Start)
        // List all grid using Neo4j
        printfn "%A %A" "startArgs:" startArgs
        Ok ()
    | argz when argz.Contains(Label) ->
        // Get the label
        let labelArgs = runArgs.GetResult(Label)
        // Get the nodes with specific label
        let result = Neo4j.getNodesByLabel(labelArgs)
        for i in result do
            printfn "%s" i
        Ok ()
    | argz when (argz.Contains(Relationship) && argz.Contains(Checksum)) ->
        // Get the relationship and checksum
        let checksum = runArgs.GetResult(Checksum)
        let relationship = runArgs.GetResult(Relationship)
        // Get the nodes with specific relationship
        let result = Neo4j.getRelatedNodes((relationship, checksum))
        for i in result do
            printfn "%s" i
        Ok ()
    | argz when argz.Contains(Relationship) ->
        // Get the relationship
        let relationship = runArgs.GetResult(Relationship)
        match relationship with
        | Some _ -> 
            printfn "%s" "No checksum of the node provided"
            Error ArgumentsNotSpecified
        | None -> 
            let result = Neo4j.getAllRelationship()
            for i in result do
                printfn "%s" i
            Ok ()
    | argz when argz.Contains(Checksum) ->
        // Get the node
        let checksum = runArgs.GetResult(Checksum)
        // Get the nodes with specific checksum
        let result = Neo4j.getNodeByChecksum(checksum)
        for i in result do
            printfn "%s" i
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
    | p when p.Contains(Init) -> runInit (p.GetResult(Init))
    | _ ->
        printfn "%s" (parser.PrintUsage())
        Error ArgumentsNotSpecified
    |> getExitCode