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
    | [<AltCommandLine("-bp")>] BasePath of msg:string option
    | [<AltCommandLine("-isd")>] InputSourceDirectory of msg:string option
    | [<AltCommandLine("-osd")>] OutputSourceDirectory of msg:string option
    | [<AltCommandLine("-od")>] OutputDirectory of msg:string option
    | [<AltCommandLine("-ca")>] CleanAll
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Init all."
            | Config _ -> "Read the config."
            | BasePath _ -> "Specifiy the base path."
            | InputSourceDirectory _ -> "Specify the input source directiory."
            | OutputSourceDirectory _ -> "Specify the output source directiory."
            | OutputDirectory _ -> "Specify the output directory."
            | CleanAll _ -> "Clean all previous nodes."

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
            // Get the base path 
            let basePathArgs = runArgs.GetResult(BasePath, Some ".")
            let basePath = defaultArg basePathArgs "."
            // Get the input source directory if any
            let inputSourceDirectoryArgs = runArgs.GetResult(InputSourceDirectory, Some "/input")
            let inputSourceDirectory = defaultArg inputSourceDirectoryArgs "/input"
            // Get the output source directory if any
            let outputSourceDirectoryArgs = runArgs.GetResult(OutputSourceDirectory, Some "/output")
            let outputSourceDirectory = defaultArg outputSourceDirectoryArgs "/output"
            // Get the ouput directory if any
            let outputDirectoryArgs = runArgs.GetResult(OutputDirectory, Some "/data")
            let outputDirectory = defaultArg outputDirectoryArgs "/data"

            // Start dealing with input
            // Get the content from the config file
            let configContent = IO.File.ReadAllText configArgs
            // Run the parser on the config file
            let configContentResult = Parser.textWithoutSpaces configContent

            match configContentResult with
            | Ok r ->
                let resultInDomain = r |> Input.parserResultToDomain
                // printfn "resultInDomain:%A" resultInDomain
                // Filter out the items that haven't defined in the domain
                let validInputResults = 
                    resultInDomain |> Array.filter (fun item -> 
                        match item with 
                        | Ok _ -> true
                        | _ -> false
                    )
                // printfn "validInputResults:%A" validInputResults
               // Unwrap the value from the result type
                let input = 
                    validInputResults 
                    |> Array.Parallel.map (fun item -> 
                        match item with 
                        | Ok v -> v
                        | Error e -> failwith e
                    )
                    |> Array.toList
                // printfn "input:%A" input

                // Delete All Previous Nodes if specified
                let shouldCleanAll = argz.Contains(CleanAll)
                if shouldCleanAll then
                    Neo4j.deleteAllNodes()
                // let result = Neo4j.createMultipleNodesIfNotExist input
                // printfn "result:%A" result
                let inputFiles = Neo4j.createAndRelateInitInputFilesFromInput input
                // printfn "inputFiles:%A" inputFiles
                // Side effect: copy input files
                match inputFiles with
                | Ok nodes -> 
                    fst nodes
                    |> filterExistedFiles (basePath, outputDirectory) 
                    |> copyInputFiles (basePath, inputSourceDirectory, outputDirectory) 
                | Error e -> failwith e

                let commitsChecksum = 
                    match inputFiles with
                    | Ok nodes -> 
                        fst nodes
                        |> Domain.getChecksumListArrayFromNodes
                    | Error e -> failwith e

                createTreeFile (basePath, outputDirectory) commitsChecksum
                // // End of dealing with input
                // // Neo4j.relateInitInputFiles input
                // // Neo4j.createInitNodesIfNotExist() |> ignore
                // // Neo4j.relateInitNodes ()

                let (checksum, _) = FileIO.getChecksumInfoFromChecksumArray commitsChecksum
                // Start dealing with output
                let (checksumDirectory, _, checksumFileName) = getPathInfoFromChecksum checksum
                let targetDirectoryWithBasePath = getFullPathWithBasePath basePath outputDirectory
                let targetWithBasePath = sprintf "%s/%s" targetDirectoryWithBasePath "output"
                let dstPath = getFullPathWithBasePath targetWithBasePath checksumDirectory
                let srcPath = getFullPathWithBasePath basePath outputSourceDirectory
                // printfn "dstPath:%A" dstPath
                // printfn "srcPath:%A" srcPath
                directoryCopy srcPath dstPath checksum false
                // Input.inputFileResult f inputDirectory
                let outputFiles = getAllFilesInDirectory dstPath
                // printfn "outputFiles:%A" outputFiles
                let outputFileNodes = Input.initOutputFileNodes outputFiles dstPath
                // printfn "outputFileNodes:%A" outputFileNodes
                // let output = 
                let result = Neo4j.createMultipleNodesIfNotExist outputFileNodes
                // printfn "result:%A" result
                Neo4j.relateOutputFilesToSimulation outputFileNodes checksum
                // End of dealing with output
                
                // Start creating directory for the calculation
                let caseTitle = "Titania"
                createCalDirectory checksum caseTitle
                // End of creating directory for the calculation
                Ok ()
            | Error e -> 
                let errorMessage = sprintf "E: %A" e
                printfn "E: %A" errorMessage
                failwith "Input parsing failed"
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