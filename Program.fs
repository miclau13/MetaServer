// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu
open Parser

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

type Data() =
    member x.Read() =
        // Read in a file with StreamReader.
        use stream = new IO.StreamReader @"/Users/miclo/MetaServer.Cli/data/input/5A/2F/F2C3D786A668D2F4DA470D2E2EDEF57B73E9-RiverNamelist"
        // Continue reading while valid lines.
        let mutable valid = true
        while (valid) do
            let line = stream.ReadLine()
            if (line = null) then
                valid <- false
            else
                // Display line.
                printfn "%A" line

let runInit (runArgs: ParseResults<InitArgs>) =
    match runArgs with
    | argz when argz.Contains(Config) ->
        // Get the config
        let configArgs = runArgs.GetResult(Config)
        try
            let newDir = IO.Directory.CreateDirectory
            let text = IO.File.ReadAllText configArgs
            let textResult = Parser.textWithoutSpaces text
            // printfn "textResult':%A" textResult
            match textResult with
            | Ok r ->
                let resultInDomain = r |> Input.parserResultToDomain
                // printfn "resultInDomain':%A" resultInDomain
                let validInputResults = 
                    resultInDomain |> Array.filter (fun item -> 
                        match item with 
                        | Ok _ -> true
                        | _ -> false
                    )
                // printfn "validInputResults':%A" validInputResults
                let input = 
                    validInputResults 
                    |> Array.Parallel.map (fun item -> 
                        match item with 
                        | Ok v -> v
                        | Error e -> failwith e
                    )
                    |> Array.toList
                printfn "input':%A" input
                
                let compressFile originalFileName targetFileName = 
                    let minimumFileSize = int64 1300000
                    let originalFileStream =  (IO.FileInfo originalFileName).OpenRead() 
                    printfn "originalFileName:%A" originalFileName
                    if originalFileStream.Length > minimumFileSize then
                        let tragetFileStream = (IO.FileInfo targetFileName).Create()

                        printfn "targetFileName:%A" targetFileName
                        let compressor = new IO.Compression.DeflateStream(tragetFileStream, IO.Compression.CompressionLevel.Optimal)
                        try 
                            originalFileStream.CopyTo(compressor)
                            let originalSize = (IO.FileInfo originalFileName).Length
                            let compressedSize = (IO.FileInfo targetFileName).Length
                            printfn "originalSize:%A\ncompressedSize:%A" originalSize compressedSize
                            tragetFileStream.Close()
                        with 
                            ex -> 
                                printfn "%s" ex.Message
                                // Error ArgumentsNotSpecified
                    originalFileStream.Close()
                let decompressFile originalFileName targetFileName = 
                    printfn "originalFileName:%A" originalFileName
                    printfn "targetFileName:%A" targetFileName
                    if IO.File.Exists originalFileName then
                        let compressedFileStream = IO.File.Open(originalFileName, IO.FileMode.Open);
                        let outputFileStream = IO.File.Create(targetFileName);
                        let decompressor = new IO.Compression.DeflateStream(compressedFileStream, IO.Compression.CompressionMode.Decompress);
                        decompressor.CopyTo(outputFileStream);
                        outputFileStream.Close()
                        compressedFileStream.Close()
                    // let originalSize = IO.FileInfo(originalFileName).Length
                    // let compressedSize = IO.FileInfo(targetFileName).Length
                    // printfn "originalSize:%A\ncompressedSize:%A" originalSize compressedSize

                let checkIfFileExist path =
                    if (IO.File.Exists path) then
                        true
                    else false
                let checkIfDirectoryExist path =
                    if (IO.Directory.Exists path) then
                        true
                    else false
                let createDirectoryIfNotExist path = 
                    if (not <| checkIfDirectoryExist path) then
                        let info = IO.Directory.CreateDirectory path
                        printfn "info: %A" info

                let copyFile basePath path (file: Domain.File) =
                    let currentDirectory = IO.Directory.GetCurrentDirectory()
                    let inputDirectory = "/data/input"
                    let fullInputDirectory = sprintf "%s%s" currentDirectory inputDirectory
                    createDirectoryIfNotExist fullInputDirectory
                    let (Domain.Checksum checksum) = file.Checksum
                    let checksumDirectory1 = checksum.[0..1]
                    let checksumDirectory2 = checksum.[2..3]
                    let checksumDirectory = sprintf "/%s/%s/" checksumDirectory1 checksumDirectory2
                    let targetDirectory = sprintf "%s%s" fullInputDirectory checksumDirectory
                    let checksumFileName = checksum.[4..]
                    createDirectoryIfNotExist targetDirectory
                    let (Domain.Name fileName) = file.Name
                    let (Domain.Format fileFormat) = file.Format
                    let sourceFileName = sprintf "%s.%s" fileName fileFormat
                    let sourceDirectory = sprintf "%s%s" currentDirectory "/input"
                    let targetFileName = sprintf "%s-%s" checksumFileName fileName
                    let sourcePath = IO.Path.Combine(sourceDirectory, sourceFileName)
                    let targetPath = IO.Path.Combine(targetDirectory, targetFileName)
                    if not <| checkIfFileExist targetPath then
                        IO.File.Copy(sourcePath, targetPath)
                    compressFile targetPath (targetPath+".gz")
                    decompressFile (targetPath+".gz") (targetPath+".ungz")

                        
                    //TODO: suppose fixed path or base path?
                Neo4j.deleteAllNodes()
                let result = Neo4j.createMultipleNodesIfNotExist input
                let inputFiles = Neo4j.createAndRelateInitInputFilesFromInput input
                match inputFiles with
                | Ok nodes -> 
                    fst nodes
                    |> Array.ofList
                    |> Array.iter (fun item -> 
                        match item with 
                        | Domain.File f -> copyFile "" "" f
                        | _ ->  printfn "Others: %A " item
                    )
                | Error e -> failwith e
                
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