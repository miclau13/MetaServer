// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu
open Parser
open FileIO

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
    | [<AltCommandLine("-c")>] Config of msg:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Init all."
            | Config _ -> "Read the config."

and ListArgs =
    | [<AltCommandLine("-a")>] All
    | [<AltCommandLine("-l")>] Label of msg:string
    | [<AltCommandLine("-r")>] Relationship of msg:string option
    | [<AltCommandLine("-cs")>] Checksum of msg:string
    | [<AltCommandLine("-mpl")>] MaxPathLength of msg:string
    | [<AltCommandLine("-rp")>] RelationshipProperty of msg:string
    | [<AltCommandLine("-rpv")>] RelationshipPropertyValue of msg:string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "List all Grids."
            | Label _ -> "Find the corresping nodes with the <label>."
            | Relationship _ -> "Find the Relationship of the node."
            | Checksum _ -> "List the node by <checksum>."
            | MaxPathLength _ -> "Specify maximum the path length."
            | RelationshipProperty _ -> "Specify Relationship property."
            | RelationshipPropertyValue _ -> "Specify Relationship property value."

let runInit (runArgs: ParseResults<InitArgs>) =
    match runArgs with
    | argz when argz.Contains(Config) ->
        // Get the config
        let configArgs = runArgs.GetResult(Config)
        try
            // Get the content from the config file
            let configContent = IO.File.ReadAllText configArgs
            // Run the parser on the config file
            let configContentResult = Parser.textWithoutSpaces configContent

            match configContentResult with
            | Ok r ->
                let resultInDomain = r |> Input.parserResultToDomain

                // Filter out the items that haven't defined in the domain
                let validInputResults = 
                    resultInDomain |> Array.filter (fun item -> 
                        match item with 
                        | Ok _ -> true
                        | _ -> false
                    )

               // Unwrap the value from the result type
                let input = 
                    validInputResults 
                    |> Array.Parallel.map (fun item -> 
                        match item with 
                        | Ok v -> v
                        | Error e -> failwith e
                    )
                    |> Array.toList

                    //TODO: suppose fixed path or base path?
                Neo4j.deleteAllNodes()
                let result = Neo4j.createMultipleNodesIfNotExist input
                let inputFiles = Neo4j.createAndRelateInitInputFilesFromInput input
                let commitsChecksum = 
                    match inputFiles with
                    | Ok nodes -> 
                        fst nodes
                        |> Array.ofList
                        |> Array.Parallel.map (fun item -> 
                            match item with 
                            | Domain.File f -> Some (copyFile (".", "/input") (".","/data") f)
                            | _ ->  None
                        )
                        |> Array.filter Option.isSome
                        |> Array.map Option.get
                    | Error e -> failwith e
                printfn "commitsChecksum:%A" commitsChecksum
                createTreeFile (".", "/data") commitsChecksum
                // Neo4j.relateInitInputFiles input
                Neo4j.createInitNodesIfNotExist() |> ignore
                Neo4j.relateInitNodes ()
                Ok ()
            | Error e -> 
                let errorMessage = sprintf "E: %A" e
                printfn "E: %A" errorMessage
                Error ArgumentsNotSpecified
        with 
            ex -> 
                printfn "%s" ex.Message
                Error ArgumentsNotSpecified
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
        printfn "Result: %A " result
        Ok ()
    | argz when argz.Contains(Label) ->
        // Get the label
        let labelArgs = runArgs.GetResult(Label)
        // Get the nodes with specific label
        let result = Neo4j.getNodesByLabel(labelArgs)
        printfn "Result: %A " result
        Ok ()
    | argz when (argz.Contains(Relationship) && argz.Contains(Checksum)) ->
        // Get the relationship and checksum
        let checksum = runArgs.GetResult(Checksum)
        // Get the nodes with specific relationship
        let relationship = runArgs.GetResult(Relationship)
        // Get the maximum path length if any
        let maxPathLength = 
            match argz.Contains(MaxPathLength) with
            | true -> sprintf "*..%s" (runArgs.GetResult(MaxPathLength))
            | false -> "*"
        let result = Neo4j.getRelatedNodesPath((relationship, checksum, maxPathLength))
        printfn "%A" result
        Ok ()
    | argz when argz.Contains(Relationship) ->
        // Get the relationship
        let relationship = runArgs.GetResult(Relationship)
        match relationship with
        | Some r -> 
            // Get the relationship properties if any
            match argz.Contains(RelationshipProperty) with
            | true -> 
                let relationshipProperty = runArgs.GetResult(RelationshipProperty)
                match argz.Contains(RelationshipPropertyValue) with
                | true -> 
                    let relationshipPropertyValue = runArgs.GetResult(RelationshipPropertyValue)
                    let result = Neo4j.getRelationships(r, Some relationshipProperty, Some relationshipPropertyValue)
                    printfn "%A" result
                    Ok ()
                    // sprintf "*..%s" (runArgs.GetResult(RelationshipProperty))
                | false ->
                    printfn "%s" "No Relationship property value provided"
                    Error ArgumentsNotSpecified
            | false ->
                // let relationshipPropertyValue = runArgs.GetResult(RelationshipPropertyValue)
                let result = Neo4j.getRelationships(r, None, None)
                printfn "%A" result
                Ok ()
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
        printfn "%A" result
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