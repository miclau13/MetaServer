// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu
open Parser
open FileIO
open Util

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

            let outputSourceFullPath = { BasePath = basePath ; RelativePath = outputSourceDirectory }
            let targetFullPath = { BasePath = basePath ; RelativePath = targetDirectory }

            // Start dealing with input
            // Get the content from the config file
            let configContent = IO.File.ReadAllText configArgs
            // Run the parser on the config file
            let configContentResult = parseContent configContent

            match configContentResult with
            | Ok r ->
                let resultInDomain = r |> Input.parserResultToDomain

                // Choose the items that have defined in the domain
                // And unwrap the value from the result type
                // Then convert to list
                let nodes = 
                    resultInDomain 
                    |> Array.choose (function | Ok v -> Some v | _ -> None)
                    |> Array.toList

                // Side Effect: Delete All Previous Nodes if specified
                let shouldCleanAll = args.Contains(CleanAll)
                if shouldCleanAll then
                    Neo4j.deleteAllNodes()

                // First, parse the input config file to get the directory of input
                let IOInput = Input.pickIOInput nodes
                let inputDirectory = Input.getIOInputDirectory IOInput
                // Next, get the input files 
                let inputFilesWithType = Input.getExistingInputFiles inputDirectory nodes
                let inputFiles = inputFilesWithType |> List.map (fun item -> item.Node)
                // Side effect: Create the files node in DB
                Neo4j.createMultipleNodesIfNotExist inputFiles

                // Side effect: copy input files
                inputFiles
                |> filterOutExistedFiles targetFullPath 
                |> copyInputFiles (basePath, inputSourceDirectory, targetDirectory) 

                // End of dealing with input

                // Start dealing with converting input config file
                let inputFilesToBeConverted = Input.getInputFilesToBeConverted inputFiles
                let inputDirToBeConverted = Input.convertConfigFileIOText inputDirectory "./"
                let inputConfigFileOutputDirectory = Input.getIOOutputDirectory IOInput
                let outputDirToBeConverted = Input.convertConfigFileIOText inputConfigFileOutputDirectory "./output/"
                let inputFilesAndIODirToBeConverted = inputFilesToBeConverted@[inputDirToBeConverted ; outputDirToBeConverted]
                let convertedConfigText = Input.convertConfigFileText configContent inputFilesAndIODirToBeConverted
                let inputConfigChecksum = getChecksum convertedConfigText
                let inputConfigFileType = "Input Config"
                // Side effect: Create the input config file
                createInputConfigFile (convertedConfigText, configArgs, inputConfigFileType, inputConfigChecksum) targetFullPath
                
                let inputConfigChecksumFileInfo = { FileName = configArgs; Checksum = inputConfigChecksum }
                let { FileDirFullPath = inputConfigFileTargetDir } = getFilePathInfo targetFullPath inputConfigChecksumFileInfo
                let inputConfigFileNameWithChecksum = getChecksumFileName inputConfigChecksum configArgs
                let inputConfigFileNode = 
                    Input.getInputFileResult inputConfigFileNameWithChecksum inputConfigFileTargetDir inputConfigFileType
                    |> Option.get
                    |> (fun item -> item.Node)

                // Side effect: Create the input config file node in DB
                Neo4j.createSingleNodeIfNotExist inputConfigFileNode

                let simulationNode = Domain.Simulation { Checksum = Domain.Checksum inputConfigChecksum }
                // Side effect: Create the relationship node in DB
                Neo4j.createSingleNodeIfNotExist simulationNode

                // Side effect: Relate the file nodes with simulation
                let inputRelationshipInfos: Neo4j.RelationShipInfo list = 
                    List.map (
                        fun (item: Input.InputFile) -> 
                            let inputFile = item.Node
                            let relationship = item.Type
                            let relationshipProps = Some (Dto.HasInputDTO { Type = relationship })
                            { SourceNode = simulationNode ; TargetNode = inputFile ; Relationship = "HAS_INPUT" ; RelationshipProps = relationshipProps }
                    ) inputFilesWithType

                let inputConfigFileRelationshipInfo: Neo4j.RelationShipInfo = 
                    { SourceNode = simulationNode ; TargetNode = inputConfigFileNode ; Relationship = "HAS_INPUT_CONFIG" ; RelationshipProps = None }

                // Done with input config file

                // Start dealing with tree file
                let commitsChecksum = inputFiles |> Domain.getChecksumListArrayFromNodes
                let _, checksumStr = getChecksumInfoFromChecksumArray commitsChecksum
                let treeFileType = "Tree"
                let treeFileName = getTreeFileName
                // Side effect: create the tree file
                createTreeFile checksumStr (treeFileName, treeFileType, inputConfigChecksum) targetFullPath

                let treeChecksumFileInfo = { FileName = treeFileName; Checksum = inputConfigChecksum }
                let { FileDirFullPath = treeFileTargetDir } = getFilePathInfo targetFullPath treeChecksumFileInfo
                let treeFileNameWithChecksum = getChecksumFileName inputConfigChecksum treeFileName
                let treeFileNode = 
                    Input.getInputFileResult treeFileNameWithChecksum treeFileTargetDir treeFileType
                    |> Option.get
                    |> (fun item -> item.Node)

                // Side effect: Create the tree file node in DB
                Neo4j.createSingleNodeIfNotExist treeFileNode

                // let treeFileRelationshipProps = Some (Dto.HasInputDTO ({ Type = inputConfigFileType }))
                let treeFileRelationshipInfo: Neo4j.RelationShipInfo = 
                    { SourceNode = simulationNode ; TargetNode = treeFileNode ; Relationship = "HAS_TREE" ; RelationshipProps = None }
                // Done with tree file

                // Start dealing with output
                // Get the target path by tree checksum
                let targetOutputFullPath = getTargetOutputFullPath targetFullPath 
                let targetOutputWithChecksumFullPath = getTargetOutputWithChecksumFullPath targetFullPath inputConfigChecksum
                // Get the source output data path 
                let srcPath = getFullPath outputSourceFullPath
                // Side effect: copy output data to the target path
                directoryCopy srcPath targetOutputWithChecksumFullPath inputConfigChecksum false

                // Get the source output data path 
                let outputFiles = getAllFilesInDirectory targetOutputWithChecksumFullPath
                let outputFileNodes = Input.initOutputFileNodes outputFiles targetOutputWithChecksumFullPath
                // Side effect: Create the output node in DB
                Neo4j.createMultipleNodesIfNotExist outputFileNodes
                // Side effect: Relate the output nodes with simulation
                let outputRelationshipInfos: Neo4j.RelationShipInfo list = 
                    List.map (
                        fun file -> 
                            { SourceNode = simulationNode ; TargetNode = file ; Relationship = "HAS_OUTPUT" ; RelationshipProps = None }
                    ) outputFileNodes

                // Side effect: append the tree file with the output files
                updateTreeRelatedFiles outputFileNodes targetFullPath inputConfigChecksum

                // End of dealing with output

                // Side effect: Relate all the nodes with simulation
                let relationshipList = inputConfigFileRelationshipInfo::treeFileRelationshipInfo::inputRelationshipInfos@outputRelationshipInfos
                Neo4j.relateMultipleNodes relationshipList
                
                // Start creating directory for the calculation

                // Get the title from FVCOMInput
                let FVCOMInputNode = 
                    nodes |>
                    List.pick (fun node -> match node with | Domain.FVCOMInput n -> Some n | _ -> None)

                let (FVCOMInput.CaseTitle caseTitle) = FVCOMInputNode.CaseTitle
                
                // Side effect: Create simulation folder
                let simulationInputFiles = inputConfigFileNode::inputFiles
                createSimulationFolder inputConfigChecksum caseTitle basePath (simulationInputFiles, targetDirectory) (outputFileNodes, targetOutputFullPath)
                
                // End of creating directory for the calculation
                
                // All side effects:
              
                
                Ok ()
            | Error e -> 
                let errorMessage = $"Input parsing failed: %A{e}"
                failwith errorMessage
            // Ok ()
        with 
            ex -> 
                printfn $"%s{ex.Message}"
                Error ArgumentsNotSpecified
    | _ ->
        Neo4j.deleteAllNodes()
        Ok ()

let runList (runArgs: ParseResults<ListArgs>) =
    match runArgs with
    | args when args.Contains(All) ->
        let result = Neo4j.getAllNodes ()
        printfn $"Result: %A{result} "
        Ok ()
    | args when args.Contains(Label) ->
        // Get the label
        let labelArgs = runArgs.GetResult(Label)
        // Get the nodes with specific label
        let result = Neo4j.getNodesByLabel(labelArgs)
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
        let result = Neo4j.getRelatedNodesPath((relationship, checksum, maxPathLength))
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
                    let result = Neo4j.getRelationships(r, Some relationshipProperty, Some relationshipPropertyValue)
                    printfn $"%A{result}"
                    Ok ()
                    // sprintf "*..%s" (runArgs.GetResult(RelationshipProperty))
                | false ->
                    printfn "%s" "No Relationship property value provided"
                    Error ArgumentsNotSpecified
            | false ->
                // let relationshipPropertyValue = runArgs.GetResult(RelationshipPropertyValue)
                let result = Neo4j.getRelationships(r, None, None)
                printfn $"%A{result}"
                Ok ()
        | None -> 
            let result = Neo4j.getAllRelationship()
            for i in result do
                printfn $"%s{i}"
            Ok ()
    | args when args.Contains(Checksum) ->
        // Get the node
        let checksum = runArgs.GetResult(Checksum)
        // Get the nodes with specific checksum
        let result = Neo4j.getNodeByChecksum(checksum)
        printfn $"%A{result}"
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
    printfn $"%s{print}"
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