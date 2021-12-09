module Input

open System.Security.Cryptography
open System
type RawInput =
    { RawString : string }

type Input = 
  | AirPressureInput of Dto.AirPressureInputDto
  | FVCOMInput of Dto.FVCOMInputDto
  | GridCoordinatesInput of Dto.GridCoordinatesInputDto
  | HeatingInput of Dto.HeatingInputDto
  | IOInput of Dto.IOInputDto
  | NetCDFInput of Dto.NetCDFInputDto
  | OBCInput of Dto.OBCInputDto
  | RiverInput of Dto.RiverInputDto
  | StartupInput of Dto.StartupInputDto
  | StartupXInput of Dto.StartupXInputDto
  | WaveInput of Dto.WaveInputDto
  | WindInput of Dto.WindInputDto
  | RawInput of RawInput

// create an active pattern
open System.Text.RegularExpressions
let (|RegexGroup|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some m.Groups.[1].Value else None
let (|RegexTitle|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some input else None

let getChecksumFromFile (path: string) =

  if IO.File.Exists(path) then
    if (IO.FileInfo(path)).Length <> 0L then
      printfn "\n%s:" path
      let content = IO.File.ReadAllBytes(path)
      let result = content |> SHA1.Create().ComputeHash |> Array.fold (fun acc b -> acc + b.ToString("X2")) ""
      result
    else
      sprintf "File %s has null length." path
  else
    sprintf "File %s does not exist." path

let getChecksum (str: string) = 
  let bytes = 
    System.Text.Encoding.UTF8.GetBytes str
    |> SHA1.Create().ComputeHash
  let result = bytes |> Array.fold (fun acc b -> acc + b.ToString("X2")) ""
  result

let getProperty (str: string) (property: string) =
    let regex = sprintf ".*?%s(?:\s*=\s*'*)([^',]*)(?:'*\s*)," property
    match str with
    | RegexGroup regex str ->
          // printfn "property: %s str:%s" property str
          str
    | _ -> "Something else"

module RawInput = 
    let toDomain (str: string) =
      let result = { RawString = str }
      result

module AirPressureInput = 
    let toDto (str: string) =
      let File = getProperty str "AIRPRESSURE_FILE"
      let Kind = getProperty str "AIRPRESSURE_KIND"
      let Checksum = getChecksum (sprintf "AIRPRESSURE_FILE=%s,AIRPRESSURE_KIND=%s" File Kind)
      let result: Dto.AirPressureInputDto = {
        File = File
        Kind = Kind
        Checksum = Checksum
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
      let Checksum = getChecksum (sprintf "CASE_TITLE=%s,TIMEZONE=%s,DATE_FORMAT=%s,START_DATE=%s,END_DATE=%s" CaseTitle TimeZone DateFormat StartDate EndDate)
      let result: Dto.FVCOMInputDto = {
        CaseTitle = CaseTitle
        TimeZone = TimeZone
        DateFormat = DateFormat
        StartDate = StartDate
        EndDate = EndDate
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.FVCOMInputDto> = {
        data = result
      }
      dto
module GridCoordinatesInput = 
    let toDto (str: string) =
      let File = getProperty str "GRID_FILE"
      let FileUnits = getProperty str "GRID_FILE_UNITS"
      let Checksum = getChecksum (sprintf "GRID_FILE=%s,GRID_FILE_UNITS=%s" File FileUnits)
      let result: Dto.GridCoordinatesInputDto = {
        File = File
        FileUnits = FileUnits
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.GridCoordinatesInputDto> = {
        data = result
      }
      dto
module HeatingInput = 
    let toDto (str: string) =
      let File = getProperty str "HEATING_FILE"
      let Type = getProperty str "HEATING_TYPE"
      let Checksum = getChecksum (sprintf "HEATING_FILE=%s,HEATING_TYPE=%s" File Type)
      let result: Dto.HeatingInputDto = {
        File = File
        Type = Type
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.HeatingInputDto> = {
        data = result
      }
      dto
module IOInput = 
    let toDto (str: string) =
      let InputDirectory = getProperty str "INPUT_DIR"
      let OutputDirectory = getProperty str "OUTPUT_DIR"
      let Checksum = getChecksum (sprintf "INPUT_DIR=%s,OUTPUT_DIR=%s" InputDirectory OutputDirectory)
      let result: Dto.IOInputDto = {
        InputDirectory = InputDirectory
        OutputDirectory = OutputDirectory
        Checksum = Checksum
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
      let Checksum = getChecksum (sprintf "NC_FIRST_OUT=%s,NC_OUT_INTERVAL=%s,NC_OUTPUT_STACK=%s" FirstOut OutInterval OutputStack)
      let result: Dto.NetCDFInputDto = {
        FirstOut = FirstOut
        OutInterval = getProperty str "NC_OUT_INTERVAL"
        OutputStack = int (getProperty str "NC_OUTPUT_STACK")
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.NetCDFInputDto> = {
        data = result
      }
      dto
module OBCInput = 
    let toDto (str: string) =
      let ElevationFile = getProperty str "OBC_ELEVATION_FILE"
      let NodeListFile = getProperty str "OBC_NODE_LIST_FILE"
      let Checksum = getChecksum (sprintf "OBC_ELEVATION_FILE=%s,OBC_NODE_LIST_FILE=%s" ElevationFile NodeListFile)
      let result: Dto.OBCInputDto = {
        ElevationFile = ElevationFile
        NodeListFile = NodeListFile
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.OBCInputDto> = {
        data = result
      }
      dto
module RiverInput = 
    let toDto (str: string) =
      let InfoFile = getProperty str "RIVER_INFO_FILE"
      let Kind = getProperty str "RIVER_KIND"
      let Number = getProperty str "RIVER_NUMBER"
      let Checksum = getChecksum (sprintf "RIVER_INFO_FILE=%s,RIVER_KIND=%s,RIVER_NUMBER=%s" InfoFile Kind Number) 
      let result: Dto.RiverInputDto = {
        InfoFile = InfoFile
        Kind = Kind
        Number = int Number
        Checksum =Checksum
      }
      let dto: Dto.Dto<Dto.RiverInputDto> = {
        data = result
      }
      dto

module StartupInput = 
    let toDto (str: string) =
      let File = getProperty str "STARTUP_FILE"
      let Type = getProperty str "STARTUP_TYPE"
      let Checksum = getChecksum (sprintf "STARTUP_FILE=%s,STARTUP_TYPE=%s" File Type)
      let result: Dto.StartupInputDto = {
        File = File
        Type = Type
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.StartupInputDto> = {
        data = result
      }
      dto

module StartupXInput = 
    let toDto (str: string) =
      let File = getProperty str "STARTUP_FILE"
      let Type = getProperty str "STARTUP_TYPE"
      let Checksum = getChecksum (sprintf "STARTUP_FILE=%s,STARTUP_TYPE=%s" File Type)
      let result: Dto.StartupXInputDto = {
        File = File
        Type = Type
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.StartupXInputDto> = {
        data = result
      }
      dto 

module WaveInput = 
    let toDto (str: string) =
      let File = getProperty str "WAVE_FILE"
      let Kind = getProperty str "WAVE_KIND"
      let Checksum = getChecksum (sprintf "WAVE_FILE=%s,WAVE_KIND=%s" File Kind)
      let result: Dto.WaveInputDto = {
        File = File
        Kind = Kind
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.WaveInputDto> = {
        data = result
      }
      dto  

module WindInput = 
    let toDto (str: string) =
      let File = getProperty str "WIND_FILE"
      let Type = getProperty str "WIND_TYPE"
      let Checksum = getChecksum (sprintf "WIND_FILE=%s,WIND_TYPE=%s" File Type)
      let result: Dto.WindInputDto = {
        File = File
        Type = Type
        Checksum = Checksum
      }
      let dto: Dto.Dto<Dto.WindInputDto> = {
        data = result
      }
      dto   

let parserResultToDomain (result: list<string>) = 
    result 
    |> Array.ofList
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
        let result = str |> OBCInput.toDto |> Dto.OBCInputDto.toDomain 
        match result with 
        | Ok r -> Array.append acc [|(Ok (Domain.OBCInput r))|]
        | Error e -> Array.append acc [|Error e|]
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
        Array.append acc [|airPressureInputResult; heatingInputResult; windInputResult; waveInputResult;|]
      | _ -> Array.append acc [|Error "No suitable toDomain"|] 
    ) Array.empty
    // |> printfn "parserResultToDomain result:%A" 
