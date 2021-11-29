module Input

open System.Security.Cryptography

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
          str
    | _ -> "Something else"

module RawInput = 
    let toDomain (str: string) =
      let result = { RawString = str }
      result

module AirPressureInput = 
    let toDto (str: string) =
      let result: Dto.AirPressureInputDto = {
        File = getProperty str "AIRPRESSURE_FILE"
        Kind = getProperty str "AIRPRESSURE_KIND"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.AirPressureInputDto> = {
        data = result
      }
      dto  
module FVCOMInput = 
    let toDto (str: string) =
      let result: Dto.FVCOMInputDto = {
        CaseTitle = getProperty str "CASE_TITLE"
        TimeZone = getProperty str "TIMEZONE"
        DateFormat = getProperty str "DATE_FORMAT"
        StartDate = getProperty str "START_DATE"
        EndDate = getProperty str "END_DATE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.FVCOMInputDto> = {
        data = result
      }
      dto
module GridCoordinatesInput = 
    let toDto (str: string) =
      let result: Dto.GridCoordinatesInputDto = {
        File = getProperty str "GRID_FILE"
        FileUnits = getProperty str "GRID_FILE_UNITS"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.GridCoordinatesInputDto> = {
        data = result
      }
      dto
module HeatingInput = 
    let toDto (str: string) =
      let result: Dto.HeatingInputDto = {
        Type = getProperty str "HEATING_TYPE"
        File = getProperty str "HEATING_FILE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.HeatingInputDto> = {
        data = result
      }
      dto
module IOInput = 
    let toDto (str: string) =
      let result: Dto.IOInputDto = {
        InputDirectory = getProperty str "INPUT_DIR"
        OutputDirectory = getProperty str "OUTPUT_DIR"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.IOInputDto> = {
        data = result
      }
      dto
module NetCDFInput = 
    let toDto (str: string) =
      let result: Dto.NetCDFInputDto = {
        FirstOut = getProperty str "NC_FIRST_OUT"
        OutInterval = getProperty str "NC_OUT_INTERVAL"
        OutputStack = int (getProperty str "NC_OUTPUT_STACK")
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.NetCDFInputDto> = {
        data = result
      }
      dto
module OBCInput = 
    let toDto (str: string) =
      let result: Dto.OBCInputDto = {
        ElevationFile = getProperty str "OBC_ELEVATION_FILE"
        NodeListFile = getProperty str "OBC_NODE_LIST_FILE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.OBCInputDto> = {
        data = result
      }
      dto
module RiverInput = 
    let toDto (str: string) =
      let result: Dto.RiverInputDto = {
        InfoFile = getProperty str "RIVER_INFO_FILE"
        Number = int (getProperty str "RIVER_NUMBER")
        Kind = getProperty str "RIVER_KIND"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.RiverInputDto> = {
        data = result
      }
      dto

module StartupInput = 
    let toDto (str: string) =
      let result: Dto.StartupInputDto = {
        Type = getProperty str "STARTUP_TYPE"
        File = getProperty str "STARTUP_FILE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.StartupInputDto> = {
        data = result
      }
      dto

module StartupXInput = 
    let toDto (str: string) =
      let result: Dto.StartupXInputDto = {
        Type = getProperty str "STARTUP_TYPE"
        File = getProperty str "STARTUP_FILE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.StartupXInputDto> = {
        data = result
      }
      dto 

module WaveInput = 
    let toDto (str: string) =
      let result: Dto.WaveInputDto = {
        File = getProperty str "WAVE_FILE"
        Kind = getProperty str "WAVE_KIND"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.WaveInputDto> = {
        data = result
      }
      dto  

module WindInput = 
    let toDto (str: string) =
      let result: Dto.WindInputDto = {
        File = getProperty str "WIND_FILE"
        Type = getProperty str "WIND_TYPE"
        Checksum = getChecksum str
      }
      let dto: Dto.Dto<Dto.WindInputDto> = {
        data = result
      }
      dto   

let parserResultToDomain (result: list<string>) = 
    result 
    |> Array.ofList
    |> Array.fold(fun acc item -> 
      // printfn "item: %s %A" item (Regex.IsMatch(item, "&NML_NETCDF\s"))
    
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
