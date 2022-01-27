module Domain 
  
open System
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

type FileWithConfigTypeOnly = {
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
  | ConfigFileInput of FileWithConfigTypeOnly
  | Simulation of Simulation
  | FVCOMInput of FVCOMInput
  | IOInput of IOInput

let getChecksumListArrayFromNodes (nodes: list<Node>)= 
  nodes
  |> Array.ofList
  |> Array.Parallel.map (fun item -> 
      match item with 
      | File f -> 
          let (Checksum checksum) = f.Checksum
          let (Name fileName) = f.Name
          match fileName with 
          | Util.RegexGroup Util.FileWithChecksumRegex 0 fileName -> 
            Some fileName
          | _ -> Some $"%s{checksum}-%s{fileName}"
      | _ ->  None
  )
  |> Array.choose id

let getFileName (file: File) =
    let (Name fileName) = file.Name
    let (Format fileFormat) = file.Format
    $"%s{fileName}.%s{fileFormat}"