module Domain 

open IOInput
open Logger
open System
open Util
type RelationShip = {
  Direction: Direction
  Node: string
}
and Direction = From | To

// All
type Checksum = Checksum of string
module Checksum =
  let create fieldName str :Result<Checksum,string> =
    if String.IsNullOrEmpty(str) then
        Error (fieldName + " must be non-empty")
    else
        Ok (Checksum str)

  let value checksum = match checksum with | Checksum c -> c

// Wave, AirPressure
type InputKind = InputKind of string
module InputKind =
  let create fieldName str :Result<InputKind, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (InputKind str)
  let value d = match d with | InputKind d -> d

// Wind, StartupX, StartupX, River, Heating
type InputType = InputType of string
module InputType =
  let create fieldName str :Result<InputType, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (InputType str)
  let value d = match d with | InputType d -> d

// Wind, Wave, StartupX, Startup, Heating, GridCoordinates, AirPressure
type InputFile = InputFile of string
module InputFile =
  let create fieldName str :Result<InputFile, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (InputFile str)
  let value d = match d with | InputFile d -> d

// Grid
type NodeNumber = NodeNumber of int
module NodeNumber =
  let create fieldName n :Result<NodeNumber, string> =
    if (n < 0) then
        Error (fieldName + " must not be negative")
    else
        Ok (NodeNumber n)
  let value nodeNumber = match nodeNumber with | NodeNumber n -> n

// File
type Path = Path of string
module Path =
  let create fieldName str :Result<Path, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (Path str)
  let value path = match path with | Path p -> p

// File
type Name = Name of string
module Name =
  let create fieldName str :Result<Name, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (Name str)
  let value name = match name with | Name n -> n

// File
type Format = Format of string
module Format =
  let create fieldName str :Result<Format, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (Format str)
  let value format = match format with | Format f -> f

type FileType = FileType of string
module FileType =
  let create fieldName str :Result<FileType, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (FileType str)
  let value format = match format with | FileType f -> f

type File = {
  Path: Path
  Name: Name
  Format: Format
  Checksum: Checksum
}

type ConfigFileInput = {
  ConfigType: FileType
  File: InputFile
}

type FVCOMInput = {
  CaseTitle: FVCOMInput.CaseTitle
  ConfigType: InputType
  DateFormat: FVCOMInput.DateFormat
  EndDate: FVCOMInput.EndDate
  StartDate: FVCOMInput.StartDate
  TimeZone: FVCOMInput.TimeZone
}

type IOInput = {
  ConfigType: InputType
  InputDirectory: IOInput.InputDirectory
  OutputDirectory: IOInput.OutputDirectory
}

type Simulation = {
  Checksum: Checksum
}

type Node = 
  | File of File
  | ConfigFileInput of ConfigFileInput
  | Simulation of Simulation
  | FVCOMInput of FVCOMInput
  | IOInput of IOInput

type RelativePath = RelativePath of string
type FullPath = FullPath of string
type BasePath = FullPath

type FileNode = {
  Node: Node
  Type: FileType
}

type PathDirection =
    | TO
    | FROM

type GetPathsByNodeInput = {
    Checksum: Checksum
    Direction: PathDirection
    MaxPathLength: string
    RelationshipOpt: string option
}

type GetPathsInput = {
    Direction: PathDirection
    Relationship: string
    RelationshipPropertyOpt: string option
    RelationshipPropertyValueOpt: string option
}

let getChecksumListArrayFromFiles (files: File list)= 
  files
  |> Array.ofList
  |> Array.Parallel.map (fun file -> 
        let (Checksum checksum) = file.Checksum
        let (Name fileName) = file.Name
        match fileName with 
        | RegexGroup FileWithChecksumRegex 0 fileName -> 
          fileName
        | _ -> $"%s{checksum}-%s{fileName}"
  )

let getIOInputDirectory (input: IOInput) = 
    let (InputDirectory dir) = input.InputDirectory 
    RelativePath dir

let checkIfFileExist (filePath: FullPath) =
    let (FullPath path) = filePath
    checkIfFileExist path
let checkIfDirectoryExist (dirPath: FullPath) =
    let (FullPath path) = dirPath
    checkIfDirectoryExist path

let getFullPath (basePath: BasePath, relativePath: RelativePath) =
    let (FullPath basePath') = basePath
    let (RelativePath relativePath') = relativePath
    let fullPath = getFullPath (basePath', relativePath')
    FullPath fullPath

let getChecksumFromFilePath (filePath: FullPath) =
    let (FullPath path) = filePath
    try
        Ok (getChecksumFromFile path)
    with
    | exn -> 
        let errorMsg = $"Cannot get checksum from file path ({path}) with error: ({exn})."
        let error = Error errorMsg
        logResult error |> ignore
        error

let tryConvertFileNodeFromConfigFile (inputDirectory: FullPath) (configFile: ConfigFileInput) =
  let (InputFile fileName) = configFile.File
  let configFileType = configFile.ConfigType
  let file = RelativePath fileName
  let (FullPath inputDir) = inputDirectory
  match fileName with
  | RegexGroup "\." 0 _ ->
    let name, format = getFileNameAndFormat fileName
    let filePath = getFullPath(inputDirectory, file)
    if checkIfFileExist filePath then
      let checksum = 
        // If input is in nc format, check if it has checksum in its file name
        // If yes then use the checksum directly, otherwise generate checksum 
        match name with 
        | RegexGroup FileWithChecksumRegex 0 name  -> 
          name
        | _ ->
            match getChecksumFromFilePath filePath with
            | Ok checksum -> checksum
            | Error error -> failwith error
      let file = File {
          Path = Path inputDir
          Name = Name name
          Format = Format format
          Checksum = Checksum checksum
      }
      Some { Node = file ; Type = configFileType }
    else 
      let errorMsg = $"File (%s{fileName}) does not exist at the path ({inputDirectory})."
      let error = Error errorMsg
      logResult error |> ignore
      failwith errorMsg
  | _ ->  
      let infoMsg = $"Input File (name: %s{fileName}, configType: {configFileType}) is not created"
      let info = Ok infoMsg
      logResult info |> ignore
      None
      
let tryGetFileNode (inputDirectory: FullPath) (node: Node) = 
    match node with 
    | Simulation _ | File _ | IOInput _ | FVCOMInput _ -> None
    | ConfigFileInput configFile -> 
        tryConvertFileNodeFromConfigFile inputDirectory configFile

let getExistingInputFiles (inputDirectory: FullPath) (inputs: Node list) =  
      let files = 
        inputs
        |> Array.ofList
        |> Array.Parallel.map (tryGetFileNode inputDirectory) 
        |> Array.toList
        |> List.choose id
      files

let getChecksumDirFromChecksum (checksum: Checksum) = 
    let (Checksum checksum') = checksum
    let directoryLevel1 = getChecksumDirFromChecksum checksum'
    RelativePath directoryLevel1

let getFileName (file: File) =
    let { Name = Name name; Format = Format format } = file
    RelativePath $"%s{name}.%s{format}"
    
let getFileNameWithChecksum (file: File) =
    let (Checksum checksum) = file.Checksum
    let (RelativePath fileName) = getFileName file
    RelativePath (getChecksumFileName checksum fileName)
    
let getChecksumFileName (fileChecksum: Checksum) (fileName: RelativePath) = 
    let (Checksum checksum) = fileChecksum
    let (RelativePath name) = fileName
    RelativePath (getChecksumFileName checksum name)

let getFileChecksumDirFullPath (fileChecksum: Checksum, destDirFullPath: FullPath) =
    let fileChecksumDir = getChecksumDirFromChecksum fileChecksum
    let fileChecksumDirFullPath = getFullPath(destDirFullPath, fileChecksumDir)
    fileChecksumDirFullPath

let getAllFilesInDirectory (fullPath: FullPath) =
    try
         let (FullPath path) = fullPath
         getAllFilesInDirectory path
         |> Array.Parallel.map (
             fun fileInfo ->
                 RelativePath fileInfo.Name
            )
    with
    | exn ->
        let errorMsg = $"getAllFilesInDirectory with path {fullPath} have error: {exn}"
        let error = Error errorMsg
        logResult error |> ignore
        failwith errorMsg

let initOutputFileNodes (files: RelativePath []) (outputDestFullDir: FullPath) (inputConfigChecksum: Checksum) = 
  let fileNodes = 
    Array.Parallel.map (fun (file: RelativePath) ->
      let (RelativePath fileName) = file
      let (Checksum checksum) = inputConfigChecksum
      let name, format = getFileNameAndFormat fileName
      let nameWithChecksum = Util.getChecksumFileName checksum name
      let (FullPath dir) = outputDestFullDir
      let result = File {
          Path = Path dir
          Name = Name nameWithChecksum
          Format = Format format
          Checksum = Checksum nameWithChecksum
      }
      result
    ) files
    |> List.ofArray
  fileNodes
  
let pickIOInput (nodes: Node list) = 
  List.pick (
      function 
      | IOInput i -> Some i
      | _ -> None
  ) nodes

let pickFVCOMInput (nodes: Node list) = 
  List.pick (
      function 
      | FVCOMInput i -> Some i
      | _ -> None
  ) nodes
  
let pickFile (nodes: Node list) = 
  List.pick (
      function 
      | File f -> Some f
      | _ -> None
  ) nodes
  
let chooseFiles (nodes: Node list) = 
  List.choose (
      function 
      | File file -> Some file
      | _ -> None
  ) nodes

