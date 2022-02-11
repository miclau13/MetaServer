// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu
open FileIO
open FVCOM.CommitTree
open FVCOM.InputConfig
open Input
open Neo4jDb
open Parser
open Util

exception ParserEx of string
type CliError =
    | ArgumentsNotSpecified
    | Neo4jError
    | ParserError
    | IOError

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
    | [<AltCommandLine("-od")>] TargetDirectory of msg:string option
    | [<AltCommandLine("-ca")>] CleanAll
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Init all."
            | Config _ -> "Read the config."
            | BasePath _ -> "Specify the base path."
            | InputSourceDirectory _ -> "Specify the input source directory."
            | OutputSourceDirectory _ -> "Specify the output source directory."
            | TargetDirectory _ -> "Specify the target directory."
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
            | Label _ -> "Find the corresponding nodes with the <label>."
            | Relationship _ -> "Find the Relationship of the node."
            | Checksum _ -> "List the node by <checksum>."
            | MaxPathLength _ -> "Specify maximum the path length."
            | RelationshipProperty _ -> "Specify Relationship property."
            | RelationshipPropertyValue _ -> "Specify Relationship property value."

let runInit (runArgs: ParseResults<InitArgs>) =
    match runArgs with
    | args when args.Contains(Config) ->
        // Get the config
        let configArgs = runArgs.GetResult(Config)
        try
            // Get the base path 
            let basePathArgs = runArgs.GetResult(BasePath, Some ".")
            let basePath = defaultArg basePathArgs "."
            // Get the input source directory if any
            let inputSourceDirectoryArgs = runArgs.GetResult(InputSourceDirectory, Some "input")
            let inputSourceDirectory = defaultArg inputSourceDirectoryArgs "input"
            // Get the output source directory if any
            let outputSourceDirectoryArgs = runArgs.GetResult(OutputSourceDirectory, Some "output")
            let outputSourceDirectory = defaultArg outputSourceDirectoryArgs "output"
            // Get the output directory if any
            let targetDirectoryArgs = runArgs.GetResult(TargetDirectory, Some "data")
            let targetDirectory = defaultArg targetDirectoryArgs "data"

            let outputSourceFullPath = getFullPath { BasePath = basePath ; RelativePath = outputSourceDirectory }
            let targetFullPath = { BasePath = basePath ; RelativePath = targetDirectory }

            // Start dealing with input
            // Get the content from the config file
            let configContent = IO.File.ReadAllText configArgs
            // Run the parser on the config file
            let configContentResult = parseContent FVCOM configContent

            match configContentResult with
            | Ok r ->
                let resultInDomain = r |> parserResultToDomain

                // Choose the items that have defined in the domain
                // And unwrap the value from the result type
                // Then convert to list
                let nodes = 
                    resultInDomain 
                    |> Array.choose (function | Ok v -> Some v | _ -> None)
                    |> Array.toList

                // First, parse the input config file to get the directory of input
                let ioInput = pickIOInput nodes
                let inputDirectory = getIOInputDirectory ioInput
                // Next, get the input files 
                let inputFilesWithType = getExistingInputFiles inputDirectory nodes
                // Start dealing with input files
                let inputFileNodes = inputFilesWithType |> List.map (fun item -> item.Node)
                let inputFiles = inputFileNodes |> chooseFiles
                // Side effect [input files]: copy input files in FS
                inputFiles
                |> filterOutExistedFiles targetFullPath 
                |> copyInputFiles (basePath, inputSourceDirectory, targetDirectory)
                // End of dealing with input

                // Start dealing with converting input config file
                let convertedConfigText = inputFiles |> getConvertedConfigText inputDirectory configContent ioInput
                let inputConfigChecksum = getChecksum convertedConfigText
                let inputConfigFileType = getInputConfigFileType ()
                // Side effect [input config file]: Create the input config file in FS
                createFile(convertedConfigText, configArgs, inputConfigFileType, inputConfigChecksum) targetFullPath
                // Done with input config file

                // Start dealing with tree file
                // Side effect [tree file]: create the tree file in FS
                createTreeFile inputConfigChecksum inputFiles targetFullPath
                // Done with tree file

                // Start dealing with output
                // Get the target path by tree checksum
                let targetOutputFullPath = getFullPath targetFullPath
                let targetOutputWithChecksumFullPath = getTargetOutputWithChecksumFullPath targetFullPath inputConfigChecksum
                // Side effect: copy output data to the target path
                directoryCopy outputSourceFullPath targetOutputWithChecksumFullPath inputConfigChecksum false
                
                // Get the source output data path 
                let outputFileInfos = getAllFilesInDirectory outputSourceFullPath
                let outputFileNodes = initOutputFileNodes outputFileInfos targetOutputWithChecksumFullPath inputConfigChecksum
                let outputFiles = outputFileNodes |> chooseFiles
                // Side effect [tree file]: append the tree file with the output files in FS
                updateTreeRelatedFiles inputConfigChecksum targetFullPath outputFiles 
                // End of dealing with output
                
                // Start creating directory for the calculation

                // Get the title from FVCOMInput
                let FVCOMInputNode = nodes |> pickFVCOMInput
                let (FVCOMInput.CaseTitle caseTitle) = FVCOMInputNode.CaseTitle
                
                let inputConfigFileNode = getInputConfigFileNode configArgs inputConfigChecksum inputConfigFileType targetFullPath
                let simulationInputFiles = inputConfigFileNode::inputFileNodes |> chooseFiles
                
                // Side effect [simulation folder]: Create the simulation folder in FS
                createSimulationFolder inputConfigChecksum caseTitle basePath (simulationInputFiles, targetDirectory) (outputFiles, targetOutputFullPath)
                // End of creating directory for the calculation
                
                // All side effects for DB:
                // Side Effect: Delete All Previous Nodes if specified in DB
                let shouldCleanAll = args.Contains(CleanAll)
                if shouldCleanAll then
                    deleteAllNodes()
                
                let simulationNode = Domain.Simulation { Checksum = Domain.Checksum inputConfigChecksum }
                let treeFileNode = getTreeFileNode inputConfigChecksum targetFullPath

                let allNodes = treeFileNode::simulationNode::inputConfigFileNode::inputFileNodes@outputFileNodes
                // Side effect [all]: Create all the nodes in DB
                createMultipleNodesIfNotExist allNodes
                
                let inputRelationshipInfos = getInputRelationshipInfos simulationNode inputFilesWithType
                let inputConfigFileRelationshipInfo = getInputConfigFileRelationshipInfo simulationNode inputConfigFileNode
                let treeFileRelationshipInfo = getTreeFileRelationshipInfo simulationNode treeFileNode
                let outputRelationshipInfos: RelationShipInfo list = 
                    List.map (
                        fun file -> 
                            { SourceNode = simulationNode ; TargetNode = file ; Relationship = "HAS_OUTPUT" ; RelationshipProps = None }
                    ) outputFileNodes
                let relationshipList = inputConfigFileRelationshipInfo::treeFileRelationshipInfo::inputRelationshipInfos@outputRelationshipInfos
                // Side effect [all]: Relate all the nodes with simulation in DB
                relateMultipleNodes relationshipList
               
                Ok ()
            | Error e -> 
                let errorMessage = $"Input parsing failed: %A{e}"
                raise (ParserEx errorMessage)
        with 
            ex ->
                match ex with
                | ParserEx errorMsg -> 
                    printfn $"%s{errorMsg}"
                    Error ParserError
                | _ ->
                    printfn $"%s{ex.Message}"
                    Error IOError
    | _ ->
        deleteAllNodes()
        Ok ()

let runList (runArgs: ParseResults<ListArgs>) =
    try 
        match runArgs with
        | args when args.Contains(All) ->
            let result = getAllNodes ()
            printfn $"Result: %A{result} "
            Ok ()
        | args when args.Contains(Label) ->
            // Get the label
            let labelArgs = runArgs.GetResult(Label)
            // Get the nodes with specific label
            let result = getNodesByLabel(labelArgs)
            printfn $"Result: %A{result} "
            Ok ()
        | args when (args.Contains(Relationship) && args.Contains(Checksum)) ->
            // Get the relationship and checksum
            let checksum = runArgs.GetResult(Checksum)
            // Get the nodes with specific relationship
            let relationship = runArgs.GetResult(Relationship)
            // Get the maximum path length if any
            let maxPathLength = 
                match args.Contains(MaxPathLength) with
                | true -> $"*..%s{runArgs.GetResult(MaxPathLength)}"
                | false -> "*"
            let result = getRelatedNodesPath((relationship, checksum, maxPathLength))
            printfn $"%A{result}"
            Ok ()
        | args when args.Contains(Relationship) ->
            // Get the relationship
            let relationship = runArgs.GetResult(Relationship)
            match relationship with
            | Some r -> 
                // Get the relationship properties if any
                match args.Contains(RelationshipProperty) with
                | true -> 
                    let relationshipProperty = runArgs.GetResult(RelationshipProperty)
                    match args.Contains(RelationshipPropertyValue) with
                    | true -> 
                        let relationshipPropertyValue = runArgs.GetResult(RelationshipPropertyValue)
                        let result = getRelationships(r, Some relationshipProperty, Some relationshipPropertyValue)
                        printfn $"%A{result}"
                        Ok ()
                    | false ->
                        printfn "%s" "No Relationship property value provided"
                        Error ArgumentsNotSpecified
                | false ->
                    // Get the relationship with all properties if not specified
                    let result = getRelationships(r, None, None)
                    printfn $"%A{result}"
                    Ok ()
            | None ->
                // Get all relationships if not specified
                let result = getAllRelationship()
                for i in result do
                    printfn $"%s{i}"
                Ok ()
        | args when args.Contains(Checksum) ->
            // Get the checksum
            let checksum = runArgs.GetResult(Checksum)
            // Get the nodes with specific checksum
            let result = getNodeByChecksum(checksum)
            printfn $"%A{result}"
            Ok ()
        | _ -> 
            printfn $"No argument provided"
            Error ArgumentsNotSpecified
    with ex ->
         printfn $"%A{ex.Message}"
         Error Neo4jError
let getExitCode result =
    match result with
    | Ok () -> 0
    | Error err ->
        match err with
        | _ -> 1

let runPrint (print: string) =
//    let fileName = FileName print
//    let file = { Name = fileName ; FullPath = FullPath ""  }
    Command.copyTestFileApi ("./input/gen_sed4.inp", "./") |> ignore
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
        printfn $"%s{parser.PrintUsage()}"
        Error ArgumentsNotSpecified
    |> getExitCode