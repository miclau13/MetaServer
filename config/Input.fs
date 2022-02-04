module Input

open System.IO

open Domain
open FileIO
open IOInput
open Util
type RawInput =
    { RawString : string }

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
  | RegexGroup "\." 0 _ ->
    let name, format = getFileNameAndFormat fileName
    let fileLocation = Path.Combine(inputDirectory, fileName)
    if (checkIfFileExist fileLocation) then
      let checksum = 
        // If input is in nc format, check if it has checksum in its file name
        // If yes then use the checksum directly, otherwise generate checksum 
        match name with 
        | RegexGroup FileWithChecksumRegex 0 name  -> 
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
      let err = $"File (%s{fileName}) does not exist at the path (%s{inputDirectory})."
      failwith err
  | _ ->  
      printfn $"Input File (name: %s{fileName}, configType: %s{fileType}) is not created "
      None

let getFilePropertyRegex (property: string) = 
  $"(.*?%s{property})(\s*=\s*'*)([^',]*)('*\s*)(,?)"

let getProperty (str: string) (property: string) =
    let regex = getFilePropertyRegex property
    match str with
    // Since the regex for file is specified
    // So the index of the capturing group must be 3 unless regex is changed
    | RegexGroup regex 3 str ->
      str
    | _ -> failwith $"Could not capture the string value (%s{str}) with property (%s{property}) by getFilePropertyRegex"

// For output files 
let initOutputFileNodes (files: FileInfo []) (dir: string) (inputConfigChecksum: string) = 
  let fileNodes = 
    Array.Parallel.map (fun (file: FileInfo) -> 
      let fileName = file.Name
      let name, format = getFileNameAndFormat fileName
      let checksum = getChecksumFileName inputConfigChecksum name
      let result = File {
          Path = Path dir
          Name = Name checksum
          Format = Format format
          Checksum = Checksum checksum
      }
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
      let file = getProperty str "AIRPRESSURE_FILE"
      let configType = sprintf "%s-%s" "NML_SURFACE_FORCING" "AIRPRESSURE_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module AMBedFlagInput = 
    let toDto (str: string) =
      let file = getProperty str "BEDFLAG_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "BEDFLAG_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module AMDataAssimilationInput = 
    let toDto (str: string) =
      let file = getProperty str "DATA_ASSIMILATION_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "DATA_ASSIMILATION_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module AMIceForcingInput = 
    let toDto (str: string) =
      let file = getProperty str "ICE_FORCING_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "ICE_FORCING_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module AMIcingForcingInput = 
    let toDto (str: string) =
      let file = getProperty str "ICING_FORCING_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "ICING_FORCING_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module AMSedimentModelInput = 
    let toDto (str: string) =
      let file = getProperty str "SEDIMENT_MODEL_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "SEDIMENT_MODEL_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module AMSedimentParameterInput = 
    let toDto (str: string) =
      let file = getProperty str "SEDIMENT_PARAMETER_FILE"
      let configType = sprintf "%s-%s" "NML_ADDITIONAL_MODELS" "SEDIMENT_PARAMETER_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
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
      let result = Dto.FVCOMInputDto{
        CaseTitle = CaseTitle
        TimeZone = TimeZone
        DateFormat = DateFormat
        StartDate = StartDate
        EndDate = EndDate
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto
module GridCoordinatesInput = 
    let toDto (str: string) =
      let File = getProperty str "GRID_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "GRID_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module GridCoriolisInput = 
    let toDto (str: string) =
      let File = getProperty str "CORIOLIS_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "CORIOLIS_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module GridDepthInput = 
    let toDto (str: string) =
      let File = getProperty str "DEPTH_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "DEPTH_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module GridSigmaLevelsInput = 
    let toDto (str: string) =
      let File = getProperty str "SIGMA_LEVELS_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "SIGMA_LEVELS_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module GridSpongeInput = 
    let toDto (str: string) =
      let File = getProperty str "SPONGE_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GRID_COORDINATES" "SPONGE_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module GroundWaterInput = 
    let toDto (str: string) =
      let File = getProperty str "GROUNDWATER_FILE"
      let ConfigType = sprintf "%s-%s" "NML_GROUNDWATER" "GROUNDWATER_FILE"
      let result = Dto.ConfigFileInputDto {
        File = File
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module HeatingCalculateInput = 
    let toDto (str: string) =
      let file = getProperty str "HEATING_CALCULATE_FILE"
      let configType = sprintf "%s-%s" "NML_HEATING_CALCULATED" "HEATING_CALCULATE_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto
module HeatingInput = 
    let toDto (str: string) =
      let file = getProperty str "HEATING_FILE"
      let configType = sprintf "%s-%s" "NML_SURFACE_FORCING" "HEATING_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto
module IOInput = 
    let toDto (str: string) =
      let InputDirectory = getProperty str "INPUT_DIR"
      let OutputDirectory = getProperty str "OUTPUT_DIR"
      let ConfigType = sprintf "%s-%s" "NML_IO" "IOInput"
      let result = Dto.IOInputDto {
        InputDirectory = InputDirectory
        OutputDirectory = OutputDirectory
        ConfigType = ConfigType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto


module LagRestartInput = 
    let toDto (str: string) =
      let file = getProperty str "LAG_RESTART_FILE"
      let configType = sprintf "%s-%s" "NML_LAG" "LAG_RESTART_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto
module LagStartInput = 
    let toDto (str: string) =
      let file = getProperty str "LAG_START_FILE"
      let configType = sprintf "%s-%s" "NML_LAG" "LAG_START_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module LagOutInput = 
    let toDto (str: string) =
      let file = getProperty str "LAG_OUT_FILE"
      let configType = sprintf "%s-%s" "NML_LAG" "LAG_OUT_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module NcnestNodeInput = 
    let toDto (str: string) =
      let file = getProperty str "NCNEST_NODE_FILES"
      let configType = sprintf "%s-%s" "NML_NCNEST" "NCNEST_NODE_FILES"
      let result = Dto.ConfigFileInputDto {
        ConfigType = configType
        File = file
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module NestingInput = 
    let toDto (str: string) =
      let file = getProperty str "NESTING_FILE_NAME"
      let configType = sprintf "%s-%s" "NML_NESTING" "NESTING_FILE_NAME"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module StationInput = 
    let toDto (str: string) =
      let file = getProperty str "STATION_FILE"
      let configType = sprintf "%s-%s" "NML_STATION_TIMESERIES" "STATION_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module OBCElevationInput = 
    let toDto (str: string) =
      let file = getProperty str "OBC_ELEVATION_FILE"
      let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_ELEVATION_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module OBCLongShoreFlowInput = 
  let toDto (str: string) =
    let file = getProperty str "OBC_LONGSHORE_FLOW_FILE"
    let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_LONGSHORE_FLOW_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto

module OBCMeanFlowInput = 
  let toDto (str: string) =
    let file = getProperty str "OBC_MEANFLOW_FILE"
    let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_MEANFLOW_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto

module OBCNodeListInput = 
  let toDto (str: string) =
    let file = getProperty str "OBC_NODE_LIST_FILE"
    let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_NODE_LIST_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto

module OBCSaltInput = 
  let toDto (str: string) =
    let file = getProperty str "OBC_SALT_FILE"
    let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_SALT_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto

module OBCTempInput = 
  let toDto (str: string) =
    let file = getProperty str "OBC_TEMP_FILE"
    let configType = sprintf "%s-%s" "NML_OPEN_BOUNDARY_CONTROL" "OBC_TEMP_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto
module PhysicsBottomRoughnessInput = 
  let toDto (str: string) =
    let file = getProperty str "BOTTOM_ROUGHNESS_FILE"
    let configType = sprintf "%s-%s" "NML_PHYSICS" "BOTTOM_ROUGHNESS_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto
module PhysicsHorizontalMixingInput = 
  let toDto (str: string) =
    let file = getProperty str "HORIZONTAL_MIXING_FILE"
    let configType = sprintf "%s-%s" "NML_PHYSICS" "HORIZONTAL_MIXING_FILE"
    let result = Dto.ConfigFileInputDto {
      File = file
      ConfigType = configType
    }
    let dto: Dto.Dto<Dto.NodeDto> = {
      data = result
    }
    dto
module PrecipitationInput = 
    let toDto (str: string) =
      let file = getProperty str "PRECIPITATION_FILE"
      let configType = sprintf "%s-%s" "NML_SURFACE_FORCING" "PRECIPITATION_FILE"
      let result = Dto.ConfigFileInputDto {
        ConfigType = configType
        File = file
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module ProbesInput = 
    let toDto (str: string) =
      let file = getProperty str "PROBES_FILE"
      let configType = sprintf "%s-%s" "NML_PROBES" "PROBES_FILE"
      let result = Dto.ConfigFileInputDto {
        ConfigType = configType
        File = file
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module RiverInput = 
    let toDto (str: string) =
      let file = getProperty str "RIVER_INFO_FILE"
      let configType = sprintf "%s-%s" "NML_RIVER_TYPE" "RIVER_INFO_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module StartupInput = 
    let toDto (str: string) =
      let file = getProperty str "STARTUP_FILE"
      let configType = sprintf "%s-%s" "NML_STARTUP" "STARTUP_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto

module StartupXInput = 
    let toDto (str: string) =
      let file = getProperty str "STARTUP_FILE"
      let configType = sprintf "%s-%s" "NML_STARTUPX" "STARTUP_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto 

module WaveInput = 
    let toDto (str: string) =
      let file = getProperty str "WAVE_FILE"
      let configType = sprintf "%s-%s" "NML_SURFACE_FORCING" "WAVE_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto  

module WindInput = 
    let toDto (str: string) =
      let file = getProperty str "WIND_FILE"
      let configType = sprintf "%s-%s" "NML_SURFACE_FORCING" "WIND_FILE"
      let result = Dto.ConfigFileInputDto {
        File = file
        ConfigType = configType
      }
      let dto: Dto.Dto<Dto.NodeDto> = {
        data = result
      }
      dto   

let getResultArrayFromNodeDto (nodeDto: Dto.Dto<Dto.NodeDto>) =
  nodeDto |> Dto.toDomain |> Array.singleton

let getNodeResultFromParserStr (parserStr: string) =
  match parserStr with
  | RegexTitle " &NML_ADDITIONAL_MODELS\s" str -> 
    let AMBedFlagInputResult = 
          str 
          |> AMBedFlagInput.toDto 
          |> getResultArrayFromNodeDto
    let AMDataAssimilationInputResult = 
          str 
          |> AMDataAssimilationInput.toDto 
          |> getResultArrayFromNodeDto
    let AMIceForcingInputResult = 
          str 
          |> AMIceForcingInput.toDto 
          |> getResultArrayFromNodeDto
    let AMIcingForcingInputResult = 
          str 
          |> AMIcingForcingInput.toDto 
          |> getResultArrayFromNodeDto
    let AMSedimentModelInputResult = 
          str 
          |> AMSedimentModelInput.toDto 
          |> getResultArrayFromNodeDto
    let AMSedimentParameterInputResult = 
          str 
          |> AMSedimentParameterInput.toDto 
          |> getResultArrayFromNodeDto
    Array.reduce Array.append [| AMBedFlagInputResult ; AMDataAssimilationInputResult ; AMIceForcingInputResult; AMIcingForcingInputResult; AMSedimentModelInputResult; AMSedimentParameterInputResult |]
  | RegexTitle "&NML_CASE\s" str -> 
    str 
    |> FVCOMInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_GRID_COORDINATES\s" str -> 
    let gridCoordinatesInputResult = 
          str 
          |> GridCoordinatesInput.toDto 
          |> getResultArrayFromNodeDto
    let gridCoriolisInputResult = 
          str 
          |> GridCoriolisInput.toDto 
          |> getResultArrayFromNodeDto
    let gridDepthInputResult = 
          str 
          |> GridDepthInput.toDto 
          |> getResultArrayFromNodeDto
    let gridSigmaLevelsInputResult = 
          str 
          |> GridSigmaLevelsInput.toDto 
          |> getResultArrayFromNodeDto
    let gridSpongeInputResult = 
          str 
          |> GridSpongeInput.toDto 
          |> getResultArrayFromNodeDto
    Array.reduce Array.append [| gridCoordinatesInputResult ; gridCoriolisInputResult ; gridDepthInputResult ; gridSigmaLevelsInputResult ; gridSpongeInputResult |]
  | RegexTitle "&NML_GROUNDWATER\s" str ->
    str 
    |> GroundWaterInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_HEATING_CALCULATED\s" str -> 
    str 
    |> HeatingCalculateInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_IO\s" str -> 
    str 
    |> IOInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_LAG\s" str -> 
    let lagOutInputResult = 
          str 
          |> LagOutInput.toDto 
          |> getResultArrayFromNodeDto
    let lagRestartInputResult = 
          str 
          |> LagRestartInput.toDto 
          |> getResultArrayFromNodeDto
    let lagStartInputResult = 
          str 
          |> LagStartInput.toDto 
          |> getResultArrayFromNodeDto
    Array.reduce Array.append [| lagOutInputResult ; lagRestartInputResult ; lagStartInputResult |]
  | RegexTitle "&NML_NCNEST\s" str ->
    str 
    |> NcnestNodeInput.toDto 
    |> getResultArrayFromNodeDto  
  | RegexTitle "&NML_NESTING\s" str ->
    let nestingInputResult = 
      str 
      |> NestingInput.toDto 
      |> getResultArrayFromNodeDto
    Array.reduce Array.append [| nestingInputResult |]
  | RegexTitle "&NML_OPEN_BOUNDARY_CONTROL\s" str -> 
    let OBCElevationInputResult = 
          str 
          |> OBCElevationInput.toDto 
          |> getResultArrayFromNodeDto
    let OBCLongShoreFlowInputResult = 
          str 
          |> OBCLongShoreFlowInput.toDto 
          |> getResultArrayFromNodeDto
    let OBCMeanFlowInputResult = 
          str 
          |> OBCMeanFlowInput.toDto 
          |> getResultArrayFromNodeDto
    let OBCNodeListInputResult = 
          str 
          |> OBCNodeListInput.toDto 
          |> getResultArrayFromNodeDto
    let OBCSaltInputResult = 
          str 
          |> OBCSaltInput.toDto 
          |> getResultArrayFromNodeDto
    let OBCTempInputResult = 
          str 
          |> OBCTempInput.toDto 
          |> getResultArrayFromNodeDto
    Array.reduce Array.append [| OBCElevationInputResult ; OBCLongShoreFlowInputResult ; OBCMeanFlowInputResult ; OBCNodeListInputResult ; OBCSaltInputResult ;OBCTempInputResult |]
  | RegexTitle "&NML_PHYSICS\s" str -> 
    let physicsBottomRoughnessInputResult = 
          str 
          |> PhysicsBottomRoughnessInput.toDto 
          |> getResultArrayFromNodeDto
    let physicsHorizontalMixingInputResult = 
          str 
          |> PhysicsHorizontalMixingInput.toDto 
          |> getResultArrayFromNodeDto
    Array.reduce Array.append [|physicsBottomRoughnessInputResult ; physicsHorizontalMixingInputResult|]
  | RegexTitle "&NML_PROBES\s" str -> 
    str 
    |> ProbesInput.toDto 
    |> getResultArrayFromNodeDto  
  | RegexTitle "&NML_RIVER_TYPE\s" str -> 
    str 
    |> RiverInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_STARTUPX\s" str ->
    str 
    |> StartupXInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_STARTUP\s" str ->
    str 
    |> StartupInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_STATION_TIMESERIES\s" str ->
    str 
    |> StationInput.toDto 
    |> getResultArrayFromNodeDto
  | RegexTitle "&NML_SURFACE_FORCING\s" str ->
    let airPressureInputResult = 
      str 
      |> AirPressureInput.toDto 
      |> getResultArrayFromNodeDto
    let heatingInputResult = 
        str 
        |> HeatingInput.toDto 
      |> getResultArrayFromNodeDto
    let precipitationInputResult = 
      str 
      |> PrecipitationInput.toDto 
      |> getResultArrayFromNodeDto
    let windInputResult = 
      str 
      |> WindInput.toDto 
      |> getResultArrayFromNodeDto
    let waveInputResult = 
      str 
      |> WaveInput.toDto 
      |> getResultArrayFromNodeDto
    
    Array.reduce Array.append [|airPressureInputResult; heatingInputResult; precipitationInputResult; windInputResult; waveInputResult |]
  | _ -> [|Error "No suitable toDto"|] 
  
let parserResultToDomain (result: list<string>) = 
    result 
    |> Array.ofList
    |> Array.Parallel.map getNodeResultFromParserStr
    |> Array.reduce Array.append

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

let getIOInputDirectory (input: IOInput) = 
    let (InputDirectory dir) = input.InputDirectory 
    dir

let getFile (inputDirectory: string) (node: Node) = 
    match node with 
    | Simulation _ | File _ | IOInput _ | FVCOMInput _-> None
    | ConfigFileInput n -> 
        let (InputFile file) = n.File
        let (FileType configType) = n.ConfigType
        getInputFileResult file inputDirectory configType

let getExistingInputFiles (inputDirectory: string) (inputs: Node list) =  
      let files = 
        inputs
        |> Array.ofList
        |> Array.Parallel.map (getFile inputDirectory) 
        |> Array.toList
        |> List.choose id
      files
