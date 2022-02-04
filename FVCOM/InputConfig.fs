module FVCOM.InputConfig

open Domain
open FileIO
open Input
open IOInput
open Neo4jDb
open Util

let getInputConfigFileType () = "Input Config"

let getInputFilesToBeConverted (inputFiles: File list) = 
    inputFiles
    |> List.map (
        fun file -> 
            let fileNameWithFormat = getFileName file
            let (Checksum checksum) = file.Checksum
            let replacementFileName = getChecksumFileName checksum fileNameWithFormat
            { Input = fileNameWithFormat ; Replacement = replacementFileName }
    )

let getConvertedConfigFileText (inputConfigText: string) (filesReplacement: InputConfigReplacement list) = 
  let convertedConfigText =
    filesReplacement
    |> List.fold (
      fun acc inputConfigReplacement -> 
        acc |> stringReplacement inputConfigReplacement.Input inputConfigReplacement.Replacement
    ) inputConfigText
  convertedConfigText

// Convert the config text to new input files, input and output Directory
let getConvertedConfigText (inputDirectory: string) (configContent: string) (ioInput: IOInput) (inputFiles: File list) =
    // Convert the config text to new input files, input and output Directory
    let inputFilesToBeConverted = getInputFilesToBeConverted inputFiles
    let inputDirToBeConverted = { Input = inputDirectory ; Replacement = "./" }
    let (OutputDirectory outputDirectory) = ioInput.OutputDirectory
    let outputDirToBeConverted = { Input = outputDirectory ; Replacement = "./output/" }
    let inputFilesAndIODirToBeConverted = inputFilesToBeConverted@[inputDirToBeConverted ; outputDirToBeConverted]
    let convertedConfigText = getConvertedConfigFileText configContent inputFilesAndIODirToBeConverted
    convertedConfigText
    
let getInputConfigFileNode (configArgs: string) (inputConfigChecksum: string) (inputConfigFileType: string) (targetFullPath: PathInfo) =
    let inputConfigChecksumFileInfo = { FileName = configArgs; Checksum = inputConfigChecksum }
    let { FileDirFullPath = inputConfigFileTargetDir } = getFilePathInfo targetFullPath inputConfigChecksumFileInfo
    let inputConfigFileNameWithChecksum = getChecksumFileName inputConfigChecksum configArgs
    let inputConfigFileNode = 
        getInputFileResult inputConfigFileNameWithChecksum inputConfigFileTargetDir inputConfigFileType
        |> Option.get
        |> (fun item -> item.Node)
    inputConfigFileNode
let getInputRelationshipInfos (simulationNode: Node) (inputFilesWithType: InputFile list) =
    let inputRelationshipInfos: RelationShipInfo list = 
        List.map (
            fun (item: Input.InputFile) -> 
                let inputFile = item.Node
                let relationship = item.Type
                let relationshipProps = Some (Dto.HasInputDTO { Type = relationship })
                { SourceNode = simulationNode ; TargetNode = inputFile ; Relationship = "HAS_INPUT" ; RelationshipProps = relationshipProps }
        ) inputFilesWithType
    inputRelationshipInfos

let getInputConfigFileRelationshipInfo  (simulationNode: Node) (inputConfigFileNode: Node) = 
    let inputConfigFileRelationshipInfo: RelationShipInfo = 
        { SourceNode = simulationNode ; TargetNode = inputConfigFileNode ; Relationship = "HAS_INPUT_CONFIG" ; RelationshipProps = None }
    inputConfigFileRelationshipInfo    