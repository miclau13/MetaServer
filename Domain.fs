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

type Simulation = {
  Checksum: Checksum
}
type Grid = {
  Checksum: Checksum
  NodeNumber: NodeNumber
}

type File = {
  Path: Path
  Name: Name
  Format: Format
  Checksum: Checksum
  // Type: FileType
}

type FileWithConfigTypeOnly = {
  ConfigType: FileType
  File: InputFile
}

// Input from Config
type AdditionalModelsDataAssimilationInput = {
  ConfigType: InputType
  File: InputFile
}

type AdditionalModelsSedimentModelInput = {
  ConfigType: InputType
  File: InputFile
}
type AdditionalModelsBedflagInput = {
  ConfigType: InputType
  File: InputFile
}
type AdditionalModelsIcingForcingInput = {
  ConfigType: InputType
  File: InputFile
}

type AdditionalModelsIceForcingInput = {
  ConfigType: InputType
  File: InputFile
}


type AirPressureInput = {
  ConfigType: InputType
  File: InputFile
  Kind: InputKind
}

type FVCOMInput = {
  CaseTitle: FVCOMInput.CaseTitle
  ConfigType: InputType
  DateFormat: FVCOMInput.DateFormat
  EndDate: FVCOMInput.EndDate
  StartDate: FVCOMInput.StartDate
  TimeZone: FVCOMInput.TimeZone
}

type GridCoordinatesInput = {
  ConfigType: InputType
  File: InputFile
  FileUnits: GridCoordinatesInput.FileUnits
}

type GridCoriolisInput = {
  ConfigType: InputType
  File: InputFile
}
type GridDepthInput = {
  ConfigType: InputType
  File: InputFile
}
type GridSigmaLevelsInput = {
  ConfigType: InputType
  File: InputFile
}
type GridSpongeInput = {
  ConfigType: InputType
  File: InputFile
}

type GroundwaterInput = {
  ConfigType: InputType
  File: InputFile
}

type HeatingInput = {
  ConfigType: InputType
  File: InputFile
  Type: InputType
}

type HeatingCalculateInput = {
  ConfigType: InputType
  File: InputFile
}

type IOInput = {
  ConfigType: InputType
  InputDirectory: IOInput.InputDirectory
  OutputDirectory: IOInput.OutputDirectory
}

type LagStartInput = {
  ConfigType: InputType
  File: InputFile
}
type LagOutInput = {
  ConfigType: InputType
  File: InputFile
}
type LagRestartInput = {
  ConfigType: InputType
  File: InputFile
}
type NcnestNodeInput = {
  ConfigType: InputType
  File: InputFile
}
type NestingFileInput = {
  ConfigType: InputType
  File: InputFile
}
type NestingStationInput = {
  ConfigType: InputType
  File: InputFile
}

type NetCDFInput = {
  ConfigType: InputType
  FirstOut: NetCDFInput.FirstOut
  OutInterval: NetCDFInput.OutInterval
  OutputStack: NetCDFInput.OutputStack
}

type OBCElevationInput = {
  ConfigType: InputType
  ElevationFile: OBCElevationInput.ElevationFile
}

type OBCLongshoreFlowInput = {
  ConfigType: InputType
  File: InputFile
}
type OBCMeanFlowInput = {
  ConfigType: InputType
  File: InputFile
}

type OBCNodeListInput = {
  ConfigType: InputType
  NodeListFile: OBCNodeListInput.NodeListFile
}

type OBCSaltInput = {
  ConfigType: InputType
  File: InputFile
}
type OBCTempInput = {
  ConfigType: InputType
  File: InputFile
}
type PhysicsHorizontalMixingInput = {
  ConfigType: InputType
  File: InputFile
}
type PhysicsBottomRoughnessInput = {
  ConfigType: InputType
  File: InputFile
}

type ProbesInput = {
  ConfigType: InputType
  File: InputFile
}
type PrecipitationInput = {
  ConfigType: InputType
  File: InputFile
  Kind: InputKind
}

type RiverInput = {
  ConfigType: InputType
  InfoFile: RiverInput.InfoFile
  Number: RiverInput.Number
  Kind: InputKind
}

type StartupInput = {
  ConfigType: InputType
  File: InputFile
  Type: InputType
}

type StartupXInput = {
  ConfigType: InputType
  File: InputFile
  Type: InputType
}

type WaveInput = {
  ConfigType: InputType
  File: InputFile
  Kind: InputKind
}
type WindInput = {
  ConfigType: InputType
  File: InputFile
  Type: InputType
}

type Node = 
  | File of File
  | ConfigFileInput of FileWithConfigTypeOnly
  // | Grid of Grid
  | Simulation of Simulation

  // | AdditionalModelsDataAssimilationInput of AdditionalModelsDataAssimilationInput
  // | AdditionalModelsSedimentModelInput of AdditionalModelsSedimentModelInput
  // | AdditionalModelsBedflagInput of AdditionalModelsBedflagInput
  // | AdditionalModelsIcingForcingInput of AdditionalModelsIcingForcingInput
  // | AdditionalModelsIceForcingInput of AdditionalModelsIceForcingInput
  // | AirPressureInput of AirPressureInput
  | FVCOMInput of FVCOMInput
  // | GridCoordinatesInput of GridCoordinatesInput
  // | GridCoriolisInput of GridCoriolisInput
  // | GridDepthInput of GridDepthInput
  // | GridSigmaLevelsInput of GridSigmaLevelsInput
  // | GridSpongeInput of GridSpongeInput
  // | GroundwaterInput of GroundwaterInput
  // | HeatingInput of HeatingInput
  // | HeatingCalculateInput of HeatingCalculateInput
  | IOInput of IOInput
  // | LagStartInput of LagStartInput
  // | LagOutInput of LagOutInput
  // | LagRestartInput of LagRestartInput
  // | NcnestNodeInput of NcnestNodeInput
  // | NestingInput of NestingFileInput
  // | NestingStationInput of NestingStationInput
  // | NetCDFInput of NetCDFInput
  // | OBCElevationInput of OBCElevationInput
  // | OBCLongshoreFlowInput of OBCLongshoreFlowInput
  // | OBCMeanFlowInput of OBCMeanFlowInput
  // | OBCNodeListInput of OBCNodeListInput
  // | OBCSaltInput of OBCSaltInput
  // | OBCTempInput of OBCTempInput
  // | PhysicsHorizontalMixingInput of PhysicsHorizontalMixingInput
  // | PhysicsBottomRoughnessInput of PhysicsBottomRoughnessInput
  // | ProbesInput of ProbesInput
  // | PrecipitationInput of PrecipitationInput
  // | RiverInput of RiverInput
  // | StartupInput of StartupInput
  // | StartupXInput of StartupXInput
  // | WaveInput of WaveInput
  // | WindInput of WindInput

let getChecksumListArrayFromNodes (nodes: list<Node>)= 
  nodes
  |> Array.ofList
  |> Array.Parallel.map (fun item -> 
      match item with 
      | File f -> 
          let (Checksum checksum) = f.Checksum
          let (Name fileName) = f.Name
          match fileName with 
          | Util.RegexGroup "(\w{40}-)(.*)" 0 fileName -> 
            Some (fileName)
          | _ -> Some (sprintf "%s-%s" checksum fileName)
      | _ ->  None
  )
  |> Array.filter Option.isSome
  |> Array.map Option.get

let getFileName (file: File) =
    let (Name fileName) = file.Name
    let (Format fileFormat) = file.Format
    sprintf "%s.%s" fileName fileFormat