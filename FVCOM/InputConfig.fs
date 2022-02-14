module FVCOM.InputConfig

open Domain
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
            let (RelativePath input) = fileNameWithFormat
            let checksum = file.Checksum
            let (RelativePath replacementFileName) = Domain.getChecksumFileName checksum fileNameWithFormat 
            { Input = input ; Replacement = replacementFileName }
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
let getConvertedConfigText (inputDirectory: RelativePath) (configContent: string) (ioInput: IOInput) (inputFiles: File list) =
    let (RelativePath inputDir) = inputDirectory
    // Convert the config text to new input files, input and output Directory
    let inputFilesToBeConverted = getInputFilesToBeConverted inputFiles
    let inputDirToBeConverted = { Input = inputDir ; Replacement = "./" }
    let (OutputDirectory outputDirectory) = ioInput.OutputDirectory
    let outputDirToBeConverted = { Input = outputDirectory ; Replacement = "./output/" }
    let inputFilesAndIODirToBeConverted = inputFilesToBeConverted@[inputDirToBeConverted ; outputDirToBeConverted]
    let convertedConfigText = getConvertedConfigFileText configContent inputFilesAndIODirToBeConverted
    convertedConfigText
    
let getInputConfigFileNode (fileName: RelativePath) (inputConfigChecksum: Checksum) (targetFullPath: FullPath) =
    let (RelativePath fileNameWithChecksum) = Domain.getChecksumFileName inputConfigChecksum fileName
    let configType = FileType (getInputConfigFileType ())
    let file = InputFile fileNameWithChecksum
    let configNode: ConfigFileInput = {
          ConfigType = configType
          File = file
    }
    let inputDir = Domain.getChecksumDirFromChecksum inputConfigChecksum
    let inputDirFullPath = Domain.getFullPath(targetFullPath, inputDir)
    let inputConfigFileNode = 
        tryConvertFileNodeFromConfigFile inputDirFullPath configNode
        |> Option.get
        |> (fun item -> item.Node)
    inputConfigFileNode
//let getInputRelationshipInfos (simulationNode: Node) (inputFilesWithType: InputFile list) =
//    let inputRelationshipInfos: RelationShipInfo list = 
//        List.map (
//            fun (item: Input.InputFile) -> 
//                let inputFile = item.Node
//                let relationship = item.Type
//                let relationshipProps = Some (Dto.HasInputDTO { Type = relationship })
//                { SourceNode = simulationNode ; TargetNode = inputFile ; Relationship = "HAS_INPUT" ; RelationshipProps = relationshipProps }
//        ) inputFilesWithType
//    inputRelationshipInfos
//
//let getInputConfigFileRelationshipInfo  (simulationNode: Node) (inputConfigFileNode: Node) = 
//    let inputConfigFileRelationshipInfo: RelationShipInfo = 
//        { SourceNode = simulationNode ; TargetNode = inputConfigFileNode ; Relationship = "HAS_INPUT_CONFIG" ; RelationshipProps = None }
//    inputConfigFileRelationshipInfo    