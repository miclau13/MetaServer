// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Argu
open Domain
open FileIO
open FileIO.CalculationDirectory
open FileIO.CommitFile
open FileIO.InputFile
open FileIO.OutputFile
open FVCOMInput
open FVCOM.CommitTree
open FVCOM.InputConfig
open Input
open Neo4jDb
open Neo4jDbHelper
open Parser
open System
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
    | [<CliPrefix(CliPrefix.None)>] TestInit of ParseResults<TestInitArgs>

with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Print _ -> "Print a message"
            | List _ -> "List node(s)"
            | Init _ -> "Init nodes and relationships"
            | TestInit _ -> "Testing"
and TestInitArgs =
    | [<AltCommandLine("-a")>] TAll
    | [<AltCommandLine("-c")>] TConfig of msg:string
    | [<AltCommandLine("-bp")>] TBasePath of msg:string option
    | [<AltCommandLine("-isd")>] TInputSourceDirectory of msg:string option
    | [<AltCommandLine("-osd")>] TOutputSourceDirectory of msg:string option
    | [<AltCommandLine("-od")>] TTargetDirectory of msg:string option
    | [<AltCommandLine("-ca")>] TCleanAll
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | TAll -> "Init all."
            | TConfig _ -> "Read the config."
            | TBasePath _ -> "Specify the base path."
            | TInputSourceDirectory _ -> "Specify the input source directory."
            | TOutputSourceDirectory _ -> "Specify the output source directory."
            | TTargetDirectory _ -> "Specify the target directory."
            | TCleanAll _ -> "Clean all previous nodes."
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
//            // Get the base path 
//            let basePathArgs = runArgs.GetResult(BasePath, Some ".")
//            let basePath = defaultArg basePathArgs "."
//            // Get the input source directory if any
//            let inputSourceDirectoryArgs = runArgs.GetResult(InputSourceDirectory, Some "input")
//            let inputSourceDirectory = defaultArg inputSourceDirectoryArgs "input"
//            // Get the output source directory if any
//            let outputSourceDirectoryArgs = runArgs.GetResult(OutputSourceDirectory, Some "output")
//            let outputSourceDirectory = defaultArg outputSourceDirectoryArgs "output"
//            // Get the output directory if any
//            let targetDirectoryArgs = runArgs.GetResult(TargetDirectory, Some "data")
//            let targetDirectory = defaultArg targetDirectoryArgs "data"
//
//            let outputSourceFullPath = FileIO.getFullPath { BasePath = basePath ; RelativePath = outputSourceDirectory }
//            let targetFullPath = { BasePath = basePath ; RelativePath = targetDirectory }
//
//            // Start dealing with input
//            // Get the content from the config file
//            let configContent = IO.File.ReadAllText configArgs
//            // Run the parser on the config file
//            let configContentResult = parseContent FVCOM configContent
//
//            match configContentResult with
//            | Ok r ->
//                let resultInDomain = r |> parserResultToDomain
//
//                // Choose the items that have defined in the domain
//                // And unwrap the value from the result type
//                // Then convert to list
//                let nodes = 
//                    resultInDomain 
//                    |> Array.choose (function | Ok v -> Some v | _ -> None)
//                    |> Array.toList
//
//                // First, parse the input config file to get the directory of input
//                let ioInput = pickIOInput nodes
//                let inputDirectory = getIOInputDirectory ioInput
//                // Next, get the input files 
//                let inputFilesWithType = getExistingInputFiles inputDirectory nodes
//                // Start dealing with input files
//                let inputFileNodes = inputFilesWithType |> List.map (fun item -> item.Node)
//                let inputFiles = inputFileNodes |> chooseFiles
//                // Side effect [input files]: copy input files in FS
//                inputFiles
//                |> filterOutExistedFiles targetFullPath 
//                |> copyInputFiles (basePath, inputSourceDirectory, targetDirectory)
//                // End of dealing with input
//
//                // Start dealing with converting input config file
//                let convertedConfigText = inputFiles |> getConvertedConfigText inputDirectory configContent ioInput
//                let inputConfigChecksum = getChecksum convertedConfigText
//                let inputConfigFileType = getInputConfigFileType ()
//                // Side effect [input config file]: Create the input config file in FS
//                createFile(convertedConfigText, configArgs, inputConfigFileType, inputConfigChecksum) targetFullPath
//                // Done with input config file
//
//                // Start dealing with tree file
//                // Side effect [tree file]: create the tree file in FS
//                createTreeFile inputConfigChecksum inputFiles targetFullPath
//                // Done with tree file
//
//                // Start dealing with output
//                // Get the target path by tree checksum
//                let targetOutputFullPath = FileIO.getFullPath targetFullPath
//                let targetOutputWithChecksumFullPath = getTargetOutputWithChecksumFullPath targetFullPath inputConfigChecksum
//                // Side effect: copy output data to the target path
//                directoryCopy outputSourceFullPath targetOutputWithChecksumFullPath inputConfigChecksum false
//                
//                // Get the source output data path 
//                let outputFileInfos = getAllFilesInDirectory outputSourceFullPath
//                let outputFileNodes = initOutputFileNodes outputFileInfos targetOutputWithChecksumFullPath inputConfigChecksum
//                let outputFiles = outputFileNodes |> chooseFiles
//                // Side effect [tree file]: append the tree file with the output files in FS
//                updateTreeRelatedFiles inputConfigChecksum targetFullPath outputFiles 
//                // End of dealing with output
//                
//                // Start creating directory for the calculation
//
//                // Get the title from FVCOMInput
//                let FVCOMInputNode = nodes |> pickFVCOMInput
//                let (FVCOMInput.CaseTitle caseTitle) = FVCOMInputNode.CaseTitle
//                
//                let inputConfigFileNode = getInputConfigFileNode configArgs inputConfigChecksum inputConfigFileType targetFullPath
//                let simulationInputFiles = inputConfigFileNode::inputFileNodes |> chooseFiles
//                
//                // Side effect [simulation folder]: Create the simulation folder in FS
//                createSimulationFolder inputConfigChecksum caseTitle basePath (simulationInputFiles, targetDirectory) (outputFiles, targetOutputFullPath)
//                // End of creating directory for the calculation
//                
//                // All side effects for DB:
//                // Side Effect: Delete All Previous Nodes if specified in DB
//                let shouldCleanAll = args.Contains(CleanAll)
//                if shouldCleanAll then
//                    deleteAllNodes()
//                
//                let simulationNode = Domain.Simulation { Checksum = Domain.Checksum inputConfigChecksum }
//                let treeFileNode = getTreeFileNode inputConfigChecksum targetFullPath
//
//                let allNodes = treeFileNode::simulationNode::inputConfigFileNode::inputFileNodes@outputFileNodes
//                // Side effect [all]: Create all the nodes in DB
//                createMultipleNodesIfNotExist allNodes
//                
//                let inputRelationshipInfos = getInputRelationshipInfos simulationNode inputFilesWithType
//                let inputConfigFileRelationshipInfo = getInputConfigFileRelationshipInfo simulationNode inputConfigFileNode
//                let treeFileRelationshipInfo = getTreeFileRelationshipInfo simulationNode treeFileNode
//                let outputRelationshipInfos: RelationShipInfo list = 
//                    List.map (
//                        fun file -> 
//                            { SourceNode = simulationNode ; TargetNode = file ; Relationship = "HAS_OUTPUT" ; RelationshipProps = None }
//                    ) outputFileNodes
//                let relationshipList = inputConfigFileRelationshipInfo::treeFileRelationshipInfo::inputRelationshipInfos@outputRelationshipInfos
//                // Side effect [all]: Relate all the nodes with simulation in DB
//                relateMultipleNodes relationshipList
//               
//                Ok ()
//            | Error e -> 
//                let errorMessage = $"Input parsing failed: %A{e}"
//                raise (ParserEx errorMessage)
             Ok ()
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
//        deleteAllNodes()
        Ok ()

let runList (runArgs: ParseResults<ListArgs>) =
    try 
        match runArgs with
        | args when args.Contains(All) ->
            Db.Neo4jDbService.getAllNodes() |> ignore
            Ok ()
        | args when args.Contains(Label) ->
            // Get the label
            let labelArgs = runArgs.GetResult(Label)
            // Get the nodes with specific label
            Db.Neo4jDbService.getNodesByLabel labelArgs |> ignore
            Ok ()
        | args when (args.Contains(Relationship) && args.Contains(Checksum)) ->
            // Get the relationship and checksum
            let checksum = Domain.Checksum (runArgs.GetResult(Checksum))
            // Get the nodes with specific relationship
            let relationshipOpt = runArgs.GetResult(Relationship)
            // Get the maximum path length if any
            let maxPathLength = 
                match args.Contains(MaxPathLength) with
                | true -> $"*..%s{runArgs.GetResult(MaxPathLength)}"
                | false -> "*"
            let pathToInput = {
                Checksum = checksum
                Direction = TO
                MaxPathLength = maxPathLength
                RelationshipOpt = relationshipOpt
            }
            let pathFromInput = {
                Checksum = checksum
                Direction = FROM
                MaxPathLength = maxPathLength
                RelationshipOpt = relationshipOpt
            }
            let pathTo = Db.Neo4jDbService.getPathsByNodeChecksum pathToInput
            let pathFrom = Db.Neo4jDbService.getPathsByNodeChecksum pathFromInput
            let path = pathTo@pathFrom
            printfn $"{path}"
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
                        let pathToInput = {
                            Direction = TO
                            Relationship = r
                            RelationshipPropertyOpt = Some relationshipProperty
                            RelationshipPropertyValueOpt = Some relationshipPropertyValue
                        }
                        let pathTo = Db.Neo4jDbService.getPaths pathToInput
                        let path = pathTo |> List.distinct
                        printfn $"{path}"
                        Ok ()
                    | false ->
                        printfn $"No Relationship property value provided"
                        Error ArgumentsNotSpecified
                | false ->
                    // Get the relationship with all properties if not specified
                    let pathToInput = {
                        Direction = TO
                        Relationship = r
                        RelationshipPropertyOpt = None
                        RelationshipPropertyValueOpt = None
                    }
                    let pathTo = Db.Neo4jDbService.getPaths pathToInput
                    let path = pathTo |> List.distinct
                    printfn $"{path}"
                    Ok ()
            | None ->
                // Get all relationships if not specified
                let relationships = Db.Neo4jDbService.getAllRelationships ()
                printfn $"{relationships}"
                Ok ()
        | args when args.Contains(Checksum) ->
            // Get the checksum
            let checksum = runArgs.GetResult(Checksum)
            // Get the nodes with specific checksum
            Db.Neo4jDbService.getNodesByChecksum (Domain.Checksum checksum) |> ignore
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

let runTest (runArgs: ParseResults<TestInitArgs>) =
    match runArgs with
    | args when args.Contains(TConfig) ->
        // Get the config
        let configArgs = runArgs.GetResult(TConfig)
        try
            // Get the base path 
            let basePathArgs = runArgs.GetResult(TBasePath, Some ".")
            let basePath = FullPath (defaultArg basePathArgs ".")
            // Get the input source directory if any
            let inputSourceDirectoryArgs = runArgs.GetResult(TInputSourceDirectory, Some "input")
            let inputSourceDirectory = RelativePath (defaultArg inputSourceDirectoryArgs "input")
            let inputSourceDirFullPath = Domain.getFullPath(basePath, inputSourceDirectory)
            // Get the output source directory if any
            let outputSourceDirectoryArgs = runArgs.GetResult(TOutputSourceDirectory, Some "output")
            let outputSourceDirectory = RelativePath (defaultArg outputSourceDirectoryArgs "output")
            let outputSourceDirectoryFullPath = Domain.getFullPath(basePath, outputSourceDirectory)
            // Get the output directory if any
            let targetDirectoryArgs = runArgs.GetResult(TTargetDirectory, Some "test")
            let targetDirectory = RelativePath (defaultArg targetDirectoryArgs "test")
            let targetDirectoryFullPath = Domain.getFullPath(basePath, targetDirectory)

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
                let inputDirFullPath = Domain.getFullPath(basePath, inputDirectory)
                // Next, get the input files 
                let fileNodes = getExistingInputFiles inputDirFullPath nodes
                // Start dealing with input files
                let inputFileNodes = fileNodes |> Array.ofList |> Array.Parallel.map (fun item -> item.Node) |> Array.toList
                let inputFiles = inputFileNodes |> chooseFiles
                // Side effect [input files]:
                // 1. Create target Dir to store the input files
                // 2. copy input files in FS
                createDirectory targetDirectoryFullPath 
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let createFilesInput = {
                    InputFiles = inputFiles
                    SourceDirPath = inputSourceDirFullPath
                    TargetDirPath = targetDirectoryFullPath
                }
                createInputFiles createFilesInput
                stopWatch.Stop()
                printfn $"Copy input files Time: %f{stopWatch.Elapsed.TotalMilliseconds}"
                // End of dealing with input
                
                // Start dealing with converting input config file
                let convertedConfigText = inputFiles |> getConvertedConfigText inputDirectory configContent ioInput
                let inputConfigChecksum = Domain.Checksum (getChecksum convertedConfigText)
                // Side effect [input config file]: Create the input config file in FS
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let inputConfigFile = RelativePath configArgs
                createFile(inputConfigFile, inputConfigChecksum, convertedConfigText, targetDirectoryFullPath)
                stopWatch.Stop()
                printfn $"Create input config files Time: %f{stopWatch.Elapsed.TotalMilliseconds}"
                // Done with input config file
                
                // Start dealing with commit file
                // Side effect [commit file]: create the commit file in FS
                let createCommitFileInput: CreateCommitFileInput = {
                    Checksum = inputConfigChecksum
                    FileTargetPath = targetDirectoryFullPath
                    InputFiles = inputFiles
                }
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                createCommitFile(createCommitFileInput)
                stopWatch.Stop()
                printfn $"Create tree files Time: %f{stopWatch.Elapsed.TotalMilliseconds}"
                // Done with commit file

                // Start dealing with output
                // Side effect: copy output data to the target path
                let outputDestChecksumDir = Domain.getChecksumDirFromChecksum inputConfigChecksum
                let outputDestFullDir = Domain.getFullPath(targetDirectoryFullPath, outputDestChecksumDir)
                let copyOutputDirInput: CopyOutputDirInput = {
                    Checksum = inputConfigChecksum
                    FilesSourcePath = outputSourceDirectoryFullPath
                    FilesTargetPath = outputDestFullDir
                }
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                copyOutputDir copyOutputDirInput
                stopWatch.Stop()
                printfn $"Copy Directory Time: %f{stopWatch.Elapsed.TotalMilliseconds}" 
                
                // Get the source output data path 
                let outputFilePaths = Domain.getAllFilesInDirectory outputSourceDirectoryFullPath
                let outputFileNodes = initOutputFileNodes outputFilePaths outputDestFullDir inputConfigChecksum
                let outputFiles = outputFileNodes |> chooseFiles
                // Side effect [tree file]: append the tree file with the output files in FS
                let updateCommitFileInput: UpdateCommitFileInput = {
                    Checksum = inputConfigChecksum
                    FileTargetPath = targetDirectoryFullPath
                    OutputFiles = outputFiles
                }
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                updateCommitFile updateCommitFileInput
                stopWatch.Stop()
                printfn $"Update Tree file Time: %f{stopWatch.Elapsed.TotalMilliseconds}" 
                // End of dealing with output
                
                // Start creating directory for the calculation
                // Get the title from FVCOMInput
                let FVCOMInputNode = nodes |> pickFVCOMInput
                let (CaseTitle caseTitle) = FVCOMInputNode.CaseTitle
                let inputConfigFileNode = getInputConfigFileNode inputConfigFile inputConfigChecksum targetDirectoryFullPath
                let simulationInputFiles = inputConfigFileNode::inputFileNodes |> chooseFiles
                // Side effect [simulation folder]: Create the simulation folder in FS
                let createSimulationDirInput: CreateSimulationDirInput = {
                    BasePath = basePath
                    CaseTitle = caseTitle
                    Checksum = inputConfigChecksum
                    FilesTargetPath = targetDirectoryFullPath
                    InputFiles = simulationInputFiles
                    OutputFiles = outputFiles
                }
                createSimulationDir createSimulationDirInput
                // End of creating directory for the calculation
               
                // All side effects for DB:
                // Side Effect: Delete All Previous Nodes if specified in DB
//                let shouldCleanAll = args.Contains(CleanAll)
//                if shouldCleanAll then
//                    deleteAllNodes()
                
                let simulationNode = Simulation { Checksum = inputConfigChecksum }
                let treeFileNode = getTreeFileNode inputConfigChecksum targetDirectoryFullPath
                let allNodes = treeFileNode::simulationNode::inputConfigFileNode::inputFileNodes@outputFileNodes
                // Side effect [all]: Create all the nodes in DB
                Db.Neo4jDbService.createNodes allNodes
     
                let inputRelationshipInfos = getInputRelationshipInfos simulationNode fileNodes
                let inputConfigFileRelationshipInfo = getInputConfigFileRelationshipInfo simulationNode inputConfigFileNode
                let treeFileRelationshipInfo = getTreeFileRelationshipInfo simulationNode treeFileNode
                let outputRelationshipInfos = getOutputRelationshipInfos simulationNode outputFileNodes
                let relationshipList = inputConfigFileRelationshipInfo::treeFileRelationshipInfo::inputRelationshipInfos@outputRelationshipInfos
                Db.Neo4jDbService.createNodesRelationship relationshipList
               
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
        let runArgsParser = ArgumentParser.Create<TestInitArgs>()
        printfn $"%s{runArgsParser.PrintUsage()}"
        Error ArgumentsNotSpecified

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CmdArgs>(programName = "metaserver", errorHandler = errorHandler)
    
    match parser.ParseCommandLine argv with
//    | p when p.Contains(Print) -> runPrint (p.GetResult(Print))
    | p when p.Contains(TestInit) -> runTest (p.GetResult(TestInit))
    | p when p.Contains(List) -> runList (p.GetResult(List))
    | p when p.Contains(Init) -> runInit (p.GetResult(Init))
    | _ ->
        printfn $"%s{parser.PrintUsage()}"
        Error ArgumentsNotSpecified
    |> getExitCode