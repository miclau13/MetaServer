module Input

open System
open System.IO

open Domain
open FileIO
open IOInput
open OBCElevationInput
open OBCNodeListInput
open RiverInput
open Util
type RawInput =
    { RawString : string }

type Input = 
  | AirPressureInput of Dto.AirPressureInputDto
  | FVCOMInput of Dto.FVCOMInputDto
  | GridCoordinatesInput of Dto.GridCoordinatesInputDto
  | HeatingInput of Dto.HeatingInputDto
  | IOInput of Dto.IOInputDto
  | NetCDFInput of Dto.NetCDFInputDto
  | OBCElevationInput of Dto.OBCElevationInputDto
  | OBCNodeListInput of Dto.OBCNodeListInputDto
  | PrecipitationInput of Dto.PrecipitationInputDto
  | RiverInput of Dto.RiverInputDto
  | StartupInput of Dto.StartupInputDto
  | StartupXInput of Dto.StartupXInputDto
  | WaveInput of Dto.WaveInputDto
  | WindInput of Dto.WindInputDto
  | RawInput of RawInput

type InputConfigReplacement = {
  Input: string
  Replacement: string
}
type InputFile = {
  Node: Node
  Type: string
}

let getFileNameAndFormat (f: string) = 
  let name = (f.Split [|'.'|]).[0]
  let format = (f.Split [|'.'|]).[1]
  (name, format)

let getInputFileResult (fileName: string) (inputDirectory: string) (fileType: string) = 
  match fileName with
  | Util.RegexGroup "\." 0 _ ->
    let (name, format) = getFileNameAndFormat fileName
    let fileLocation = Path.Combine(inputDirectory, fileName)
    if (FileIO.checkIfFileExist fileLocation) then
      let checksum = 
        // If input is in nc format, check if it has checksum in its file name
        // If yes then use the checksum directly, otherwise generate checksum 
        match name with 
        | Util.RegexGroup "(\w{40}-)(.*)" 0 name  -> 
          name
        | _ -> getChecksumFromFile fileLocation
      let file = File {
          Path = Path inputDirectory
          Name = Name name
          Format = Format format
          Checksum = Checksum checksum
      }
      Some { Node = file ; Type = fileType }
    else 
      let err = sprintf "File (%s) does not exist at the path (%s)." fileName inputDirectory
      failwith err
  | _ ->  
      printfn "Input File (%s) is not created " fileName
      None

let getFilePropertyRegex (property: string) = 
  sprintf "(.*?%s)(\s*=\s*'*)([^',]*)('*\s*)(,?)" property

let getProperty (str: string) (property: string) =
    let regex = getFilePropertyRegex property
    match str with
    // Since the regex for file is specified
    // So the index of the capturing group must be 3 unless regex is changed
    | Util.RegexGroup regex 3 str ->
      str
    | _ -> failwith "Could not capture the value by getFilePropertyRegex"

// For output files 
let initOutputFileNodes (files: IO.FileInfo []) (dir: string) = 
  let fileNodes = 
    Array.Parallel.map (fun (file: IO.FileInfo) -> 
      let fileName = file.Name
      let name = (fileName.Split [|'.'|]).[0]
      let format = (fileName.Split [|'.'|]).[1]
      // let fileLocation = sprintf "%s%s" dir fileName
      let checksum = 
        // use file name as the checksum
        name
        // getChecksum fileName
      let result = File {
          Path = Path dir
          Name = Name name
          Format = Format format
          Checksum = Checksum (checksum)
          // Type = FileType "File"
      }
      // printfn "initOutputFileNodes result: %A" result
      result
    ) files
    |> List.ofArray
  fileNodes
module RawInput = 
    let toDomain (str: string) =
      let result = { RawString = str }
      result

module AirPressureInput = 
    let toDto (str: string) =
      let File = getProperty str "AIRPRESSURE_FILE"
      let Kind = getProperty str "AIRPRESSURE_KIND"
      let ConfigType = sprintf "%s-%s" "NML_SURFACE_FORCING" "AIRPRESSURE_FILE"
      let result: Dto.AirPressureInputDto = {
        File = File
        Kind = Kind
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.AirPressureInputDto> = {
        data = result
      }
      dto  
module FVCOMInput = 
    let toDto (str: string) =
      let CaseTitle = getProperty str "CASE_TITLE"
      let TimeZone = getProperty str "TIMEZONE"
      let DateFormat = getProperty str "DATE_FORMAT"
      let StartDate = getProperty str "START_DATE"
      let EndDate = getProperty str "END_DATE"
      let ConfigType = sprintf "%s-%s" "NML_CASE" "FVCOMInput"
      let result: Dto.FVCOMInputDto = {
        CaseTitle = CaseTitle
        TimeZone = TimeZone
        DateFormat = DateFormat
        StartDate = StartDate
        EndDate = EndDate
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.FVCOMInputDto> = {
        data = result
      }
      dto
module GridCoordinatesInput = 
    let toDto (str: string) =
      let File = getProperty str "GRID_FILE"
      let FileUnits = getProperty str "GRID_FILE_UNITS"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "GRID_FILE"
      let result: Dto.GridCoordinatesInputDto = {
        File = File
        FileUnits = FileUnits
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.GridCoordinatesInputDto> = {
        data = result
      }
      dto
module HeatingInput = 
    let toDto (str: string) =
      let File = getProperty str "HEATING_FILE"
      let Type = getProperty str "HEATING_TYPE"
      let ConfigType = sprintf "%s-%s" "NML_SURFACE_FORCING" "HEATING_FILE"
      let result: Dto.HeatingInputDto = {
        File = File
        Type = Type
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.HeatingInputDto> = {
        data = result
      }
      dto
module IOInput = 
    let toDto (str: string) =
      let InputDirectory = getProperty str "INPUT_DIR"
      let OutputDirectory = getProperty str "OUTPUT_DIR"
      let ConfigType = sprintf "%s-%s" "NML_IO" "IOInput"
      let result: Dto.IOInputDto = {
        InputDirectory = InputDirectory
        OutputDirectory = OutputDirectory
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.IOInputDto> = {
        data = result
      }
      dto
module NetCDFInput = 
    let toDto (str: string) =
      let FirstOut = getProperty str "NC_FIRST_OUT"
      let OutInterval = getProperty str "NC_OUT_INTERVAL"
      let OutputStack = getProperty str "NC_OUTPUT_STACK"
      let ConfigType = sprintf "%s-%s" "NML_NETCDF" "NetCDFInput"
      let result: Dto.NetCDFInputDto = {
        FirstOut = FirstOut
        OutInterval = OutInterval
        OutputStack = int OutputStack
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NetCDFInputDto> = {
        data = result
      }
      dto
module OBCElevationInput = 
    let toDto (str: string) =
      let ElevationFile = getProperty str "OBC_ELEVATION_FILE"
      let NodeListFile = getProperty str "OBC_NODE_LIST_FILE"
      let ConfigType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_ELEVATION_FILE"
      let result: Dto.OBCElevationInputDto = {
        ElevationFile = ElevationFile
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.OBCElevationInputDto> = {
        data = result
      }
      dto

module OBCNodeListInput = 
    let toDto (str: string) =
      let NodeListFile = getProperty str "OBC_NODE_LIST_FILE"
      let ConfigType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_NODE_LIST_FILE"
      let result: Dto.OBCNodeListInputDto = {
        NodeListFile = NodeListFile
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.OBCNodeListInputDto> = {
        data = result
      }
      dto
module PrecipitationInput = 
    let toDto (str: string) =
      let File = getProperty str "PRECIPITATION_FILE"
      let Kind = getProperty str " PRECIPITATION_KIND"
      let ConfigType = sprintf "%s-%s" "NML_SURFACE_FORCING" "PRECIPITATION_FILE"
      let result: Dto.PrecipitationInputDto = {
        ConfigType = ConfigType
        File = File
        Kind = Kind
      }
      let dto: Dto.Dto<Dto.PrecipitationInputDto> = {
        data = result
      }
      dto

module RiverInput = 
    let toDto (str: string) =
      let InfoFile = getProperty str "RIVER_INFO_FILE"
      let Kind = getProperty str "RIVER_KIND"
      let Number = getProperty str "RIVER_NUMBER"
      let ConfigType = sprintf "%s-%s" "NML_RIVER_TYPE" "RIVER_INFO_FILE"
      let result: Dto.RiverInputDto = {
        InfoFile = InfoFile
        Kind = Kind
        Number = int Number
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.RiverInputDto> = {
        data = result
      }
      dto

module StartupInput = 
    let toDto (str: string) =
      let File = getProperty str "STARTUP_FILE"
      let Type = getProperty str "STARTUP_TYPE"
      let ConfigType = sprintf "%s-%s" "NML_STARTUP" "STARTUP_FILE"
      let result: Dto.StartupInputDto = {
        File = File
        Type = Type
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.StartupInputDto> = {
        data = result
      }
      dto

module StartupXInput = 
    let toDto (str: string) =
      let File = getProperty str "STARTUP_FILE"
      let Type = getProperty str "STARTUP_TYPE"
      let ConfigType = sprintf "%s-%s" "NML_STARTUPX" "STARTUP_FILE"
      let result: Dto.StartupXInputDto = {
        File = File
        Type = Type
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.StartupXInputDto> = {
        data = result
      }
      dto 

module WaveInput = 
    let toDto (str: string) =
      let File = getProperty str "WAVE_FILE"
      let Kind = getProperty str "WAVE_KIND"
      let ConfigType = sprintf "%s-%s" "NML_SURFACE_FORCING" "WAVE_FILE"
      let result: Dto.WaveInputDto = {
        File = File
        Kind = Kind
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.WaveInputDto> = {
        data = result
      }
      dto  

module WindInput = 
    let toDto (str: string) =
      let File = getProperty str "WIND_FILE"
      let Type = getProperty str "WIND_TYPE"
      let ConfigType = sprintf "%s-%s" "NML_SURFACE_FORCING" "WIND_FILE"
      let result: Dto.WindInputDto = {
        File = File
        Type = Type
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.WindInputDto> = {
        data = result
      }
      dto   

let parserResultToDomain (result: list<string>) = 
    result 
    |> Array.ofList
    // TODO parallel collect to array of array then flatten
    // |> Array.Parallel
    |> Array.fold(fun acc item -> 
      // printfn "item: %s" item 
      match item with 
      | RegexTitle "&NML_CASE\s" str -> 
        let result = str |> FVCOMInput.toDto |> Dto.FVCOMInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.FVCOMInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_GRID_COORDINATES\s" str -> 
        let result = str |> GridCoordinatesInput.toDto |> Dto.GridCoordinatesInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.GridCoordinatesInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_IO\s" str -> 
        let result = str |> IOInput.toDto |> Dto.IOInputDto.toDomain 
        // printfn "NML_IO str: %s , result: %A" str result
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.IOInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_NETCDF\s" str -> 
        let result = str |> NetCDFInput.toDto |> Dto.NetCDFInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.NetCDFInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_OPEN_BOUNDARY_CONTROL\s" str -> 
        let OBCElevationInputResult = 
          str 
          |> OBCElevationInput.toDto 
          |> Dto.OBCElevationInputDto.toDomain 
          |> function
            | Ok r -> Ok (Domain.OBCElevationInput r)
            | Error e -> Error e
        let OBCNodeListInputResult = 
          str 
          |> OBCNodeListInput.toDto 
          |> Dto.OBCNodeListInputDto.toDomain 
          |> function
            | Ok r -> Ok (Domain.OBCNodeListInput r)
            | Error e -> Error e
        Array.append acc [|OBCElevationInputResult; OBCNodeListInputResult|]
      | RegexTitle "&NML_RIVER_TYPE\s" str -> 
        let result = str |> RiverInput.toDto |> Dto.RiverInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.RiverInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_STARTUPX\s" str ->
        let result = str |> StartupXInput.toDto |> Dto.StartupXInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.StartupXInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_STARTUP\s" str ->
        let result = str |> StartupInput.toDto |> Dto.StartupInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.StartupInput r))|]
        | Error e -> Array.append acc [|Error e|]
      | RegexTitle "&NML_SURFACE_FORCING\s" str ->
        let airPressureInputResult = 
          str 
          |> AirPressureInput.toDto 
          |> Dto.AirPressureInputDto.toDomain
          |> function
            | Ok r -> Ok (Domain.AirPressureInput r)
            | Error e -> Error e
        let heatingInputResult = 
            str 
            |> HeatingInput.toDto 
            |> Dto.HeatingInputDto.toDomain 
            |> function
              | Ok r -> Ok (Domain.HeatingInput r)
              | Error e -> Error e
        let precipitationInputResult = 
          str 
          |> PrecipitationInput.toDto 
          |> Dto.PrecipitationInputDto.toDomain 
          |> function
            | Ok r -> Ok (Domain.PrecipitationInput r)
            | Error e -> Error e
        let windInputResult = 
          str 
          |> WindInput.toDto 
          |> Dto.WindInputDto.toDomain 
          |> function
            | Ok r -> Ok (Domain.WindInput r)
            | Error e -> Error e
        let waveInputResult = 
          str 
          |> WaveInput.toDto 
          |> Dto.WaveInputDto.toDomain 
          |> function
            | Ok r -> Ok (Domain.WaveInput r)
            | Error e -> Error e
        Array.append acc [|airPressureInputResult; heatingInputResult; precipitationInputResult; windInputResult; waveInputResult;|]
      | _ -> Array.append acc [|Error "No suitable toDomain"|] 
    ) Array.empty
    // |> printfn "parserResultToDomain result:%A" 

let pickIOInput (nodes: Node list) = 
  List.pick (
      function 
      | Domain.IOInput i -> Some i
      | _ -> None
  ) nodes

let getIOInputDirectory (input: IOInput) = 
    let (InputDirectory dir) = input.InputDirectory 
    dir

let getIOOutputDirectory (input: IOInput) = 
    let (OutputDirectory dir) = input.OutputDirectory 
    dir

// let getFileResult (inputDirectory: string) (node: Node) = 
//     match node with 
//     | Simulation _ -> None
//     | Grid _ -> None
//     | File _ -> None
//     | Domain.AirPressureInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "AirPressureInput"
//     | Domain.FVCOMInput _ ->
//         None
//     | Domain.GridCoordinatesInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "GridCoordinatesInput"
//     | Domain.HeatingInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "HeatingInput"
//     | Domain.IOInput _ -> 
//         None
//     | Domain.NetCDFInput _ -> 
//         None
//     | Domain.OBCElevationInput n -> 
//         let (ElevationFile file) = n.ElevationFile
//         getInputFileResult file inputDirectory "OBCElevationInput"
//     | Domain.OBCNodeListInput n -> 
//         let (NodeListFile file) = n.NodeListFile
//         getInputFileResult file inputDirectory "OBCNodeListInput"
//     | Domain.RiverInput n -> 
//         let (InfoFile file) = n.InfoFile
//         getInputFileResult file inputDirectory "RiverInput"
//     | Domain.StartupInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "StartupInput"
//     | Domain.StartupXInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "StartupXInput"
//     | Domain.WaveInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "WaveInput"
//     | Domain.WindInput n -> 
//         let (InputFile file) = n.File
//         getInputFileResult file inputDirectory "WindInput"

// let findExistingInputFilesWithType (inputDirectory: string) (inputs: Node list) =  
//       let files = List.map (getFileResult inputDirectory) inputs
//       let inputFilesWithType = files |> List.choose id
//       inputFilesWithType

let getFile (inputDirectory: string) (node: Node) = 
    match node with 
    | Simulation _ | Grid _ | File _ | Domain.IOInput _ | Domain.NetCDFInput _ | Domain.FVCOMInput _-> None
    | Domain.AirPressureInput n -> 
        let (InputFile file) = n.File
        let (InputType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType
    | Domain.GridCoordinatesInput n -> 
        let (InputFile file) = n.File
        let (InputType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType
    | Domain.HeatingInput n -> 
        let (InputFile file) = n.File
        let (InputType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType
    | Domain.OBCElevationInput n -> 
        let (ElevationFile file) = n.ElevationFile
        let (InputType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType
    | Domain.OBCNodeListInput n -> 
        let (NodeListFile file) = n.NodeListFile
        let (InputType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType
    | Domain.PrecipitationInput n -> 
        let (InputFile file) = n.File
        let (InputType fileType) = n.ConfigType
        getInputFileResult file inputDirectory fileType
    | Domain.RiverInput n -> 
        let (InfoFile file) = n.InfoFile
        getInputFileResult file inputDirectory "RiverInput"
    | Domain.StartupInput n -> 
        let (InputFile file) = n.File
        getInputFileResult file inputDirectory "StartupInput"
    | Domain.StartupXInput n -> 
        let (InputFile file) = n.File
        getInputFileResult file inputDirectory "StartupXInput"
    | Domain.WaveInput n -> 
        let (InputFile file) = n.File
        getInputFileResult file inputDirectory "WaveInput"
    | Domain.WindInput n -> 
        let (InputFile file) = n.File
        getInputFileResult file inputDirectory "WindInput"

let getExistingInputFiles (inputDirectory: string) (inputs: Node list) =  
      let files = 
        inputs
        |> Array.ofList
        |> Array.Parallel.map (getFile inputDirectory) 
        |> Array.toList
        |> List.choose id
      files

let getInputFilesToBeConverted (inputFiles: Domain.Node list) = 
    inputFiles
    |> List.choose (fun node -> match node with | Domain.File file -> Some file | _ -> None)
    |> List.map (
        fun file -> 
            let fileNameWithFormat = getFileName file
            let (Domain.Checksum checksum) = file.Checksum
            let replacementFileName = getChecksumFileName checksum fileNameWithFormat
            { Input = fileNameWithFormat ; Replacement = replacementFileName }
    )
  
let convertConfigFileText (inputConfigText: string) (filesReplacement: InputConfigReplacement list) = 
  let convertedConfigText =
    filesReplacement
    |> List.fold (
      fun acc inputConfigReplacement -> 
        acc |> Util.stringReplacement inputConfigReplacement.Input inputConfigReplacement.Replacement
    ) inputConfigText
  convertedConfigText

let convertConfigFileIOText (dir: string) (replacement: string) =
  { Input = dir ; Replacement = replacement }