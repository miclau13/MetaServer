module Dto 

open Domain
open System
open FVCOMInput
open IOInput

type Dto<'T> = {
  data: 'T
}

// Input Dto from config
type SimulationDto = {
  Checksum: string
}

type ConfigFileInputDto = {
  ConfigType: string
  File: string
}

type FileDto = {
  Path: string
  Name: string
  Format: string
  Checksum: string
}
type FVCOMInputDto = {
  CaseTitle: String
  ConfigType: string
  DateFormat: string
  EndDate: string
  StartDate: string
  TimeZone: string
}
type IOInputDto = {
  ConfigType: string
  InputDirectory: string
  OutputDirectory: string
}

type HasInputDTO = {
  Type: string
}

type FileLocationIsDTO = {
  BasicPath: string
}
type RelationshipDto = 
  | HasInputDTO of HasInputDTO
  | FileLocationIsDTO of FileLocationIsDTO
  | HasOutputDTO 
  | HasInputConfigDTO 
  | HasTreeDTO 

/// Define a type to represent possible errors
type DtoError =
    | ValidationError of string
    | DeserializationException of exn

type NodeDto = 
  | FileDto of FileDto
  | ConfigFileInputDto of ConfigFileInputDto
  | IOInputDto of IOInputDto
  | FVCOMInputDto of FVCOMInputDto
  | SimulationDto of SimulationDto

type NodeOutput = {
  Labels: string []
  Nodes: Result<Node, DtoError> option []
}

type ResultBuilder() =
    member this.Return x = Ok x
    member this.Zero() = Ok ()
    member this.Bind(xResult,f) = Result.bind f xResult

let result = ResultBuilder()

let toDto (data: 'T) = 
  { data = data }

let fromDto (dto: Dto<'T>) = 
  dto.data

// Convert a node into a node dto
let fromDomain (inputNode: Domain.Node) =
  match inputNode with 
  | Domain.File file -> 
    let checksum = file.Checksum |> Checksum.value
    let name = file.Name |> Name.value
    let path = file.Path |> Path.value
    let format = file.Format |> Format.value
    let dto = FileDto { Checksum = checksum; Name = name; Path = path; Format = format } 
    dto
  | Domain.ConfigFileInput input -> 
      let configType = input.ConfigType |> FileType.value
      let file = input.File |> InputFile.value
      let dto = ConfigFileInputDto {
        ConfigType = configType
        File = file
      } 
      dto
  | Domain.FVCOMInput fvcomInput -> 
    let configType = fvcomInput.ConfigType |> InputType.value
    let startDate = fvcomInput.StartDate |> StartDate.value
    let endDate = fvcomInput.EndDate |> EndDate.value
    let caseTitle = fvcomInput.CaseTitle |> CaseTitle.value
    let timezone = fvcomInput.TimeZone |> TimeZone.value
    let dateFormat = fvcomInput.DateFormat |> DateFormat.value
    let dto = FVCOMInputDto { 
      ConfigType = configType
      CaseTitle = caseTitle
      DateFormat = dateFormat
      EndDate = endDate
      StartDate = startDate
      TimeZone = timezone
    }
    dto
  | Domain.IOInput input -> 
      let configType = input.ConfigType |> InputType.value
      let inputDirectory = input.InputDirectory |> InputDirectory.value
      let outputDirectory = input.OutputDirectory |> OutputDirectory.value
      let dto = IOInputDto {
        ConfigType = configType
        InputDirectory = inputDirectory
        OutputDirectory = outputDirectory
      }
      dto
  | Domain.Simulation input -> 
      let checksum = input.Checksum |> Checksum.value
      let dto = SimulationDto {
        Checksum = checksum
      } 
      dto

// create a domain object from a DTO
let toDomain (dto: Dto<NodeDto>) :Result<Domain.Node,string> =
  let nodeDto = dto |> fromDto
  match nodeDto with 
  | FileDto data -> 
      result {
        // get each (validated) simple type from the DTO as a success or failure
        let! checksum = data.Checksum |> Checksum.create "Checksum"
        let! name = data.Name |> Name.create "Name"
        let! path = data.Path |> Path.create "Path"
        let! format = data.Format |> Format.create "Format"
        // combine the components to create the domain object
        return Domain.File {
          Checksum = checksum
          Name = name
          Path = path
          Format = format
        }
      }
  | ConfigFileInputDto data -> 
        result {
        let! configType = data.ConfigType |> FileType.create "ConfigType"
        let! file = data.File |> InputFile.create "File"
        return Domain.ConfigFileInput {
          ConfigType = configType
          File = file
        }
      }
  | FVCOMInputDto data -> 
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! configType = data.ConfigType |> InputType.create "ConfigType"
      let! startDate = data.StartDate |> StartDate.create "StartDate"
      let! endDate = data.EndDate |> EndDate.create "EndDate"
      let! caseTitle = data.CaseTitle |> CaseTitle.create "CaseTitle"
      let! timezone = data.TimeZone |> TimeZone.create "TimeZone"
      let! dateFormat = data.DateFormat |> DateFormat.create "DateFormat"
      // combine the components to create the domain object
      return Domain.FVCOMInput {
        ConfigType = configType
        CaseTitle = caseTitle
        DateFormat = dateFormat
        EndDate = endDate
        StartDate = startDate
        TimeZone = timezone
      }
    }
  | IOInputDto data -> 
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! configType = data.ConfigType |> InputType.create "ConfigType"
      let! inputDirectory = data.InputDirectory |> InputDirectory.create "InputDirectory"
      let! outputDirectory = data.OutputDirectory |> OutputDirectory.create "OutputDirectory"
      // combine the components to create the domain object
      return Domain.IOInput {
        ConfigType = configType
        InputDirectory = inputDirectory
        OutputDirectory = outputDirectory
      }
    }
  | SimulationDto data -> 
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      // combine the components to create the domain object
      return Domain.Simulation {
        Checksum = checksum
      }
    }

// Serialize the Node into a JSON string
let jsonFromDomain (input: Domain.Node) =
  let nodeDto = input |> fromDomain
  match nodeDto with 
  | FileDto dto -> dto |> Json.serialize
  | ConfigFileInputDto dto -> dto |> Json.serialize
  | FVCOMInputDto dto -> dto |> Json.serialize
  | IOInputDto dto -> dto |> Json.serialize
  | SimulationDto dto -> dto |> Json.serialize


let getNodeFromDtoData (dtoData: Dto<NodeDto>) = 
    dtoData
    |> toDomain
    |> Result.mapError ValidationError
module FVCOMInputDto =
  // Deserialize a JSON string into a FVCOMInput Node
  let jsonToDomain jsonString :Result<Domain.Node, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<FVCOMInputDto>>
          |> Result.mapError DeserializationException
      let fvcomInputDto = deserializedValue.data
      let nodeDto = FVCOMInputDto fvcomInputDto
      let nodeDtoData = {
        data = nodeDto
      }
      let! domainValue =
          nodeDtoData
          |> getNodeFromDtoData

      return domainValue
  }
module FileDto =
  // Deserialize a JSON string into a File Node
  let jsonToDomain jsonString :Result<Domain.Node, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<FileDto>>
          |> Result.mapError DeserializationException
      let fileDto = deserializedValue.data
      let nodeDto = FileDto fileDto
      let nodeDtoData = {
        data = nodeDto
      }
      let! domainValue =
          nodeDtoData
          |> getNodeFromDtoData

      return domainValue
  }

module IOInputDto =
  // Deserialize a JSON string into a IOInput Node
  let jsonToDomain jsonString :Result<Domain.Node, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<IOInputDto>>
          |> Result.mapError DeserializationException
      let ioInputDto = deserializedValue.data
      let nodeDto = IOInputDto ioInputDto
      let nodeDtoData = {
        data = nodeDto
      }
      let! domainValue =
          nodeDtoData
          |> getNodeFromDtoData

      return domainValue
  }

module SimulationDto =
  // Deserialize a JSON string into a Simulation Node
  let jsonToDomain jsonString :Result<Domain.Node, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<SimulationDto>>
          |> Result.mapError DeserializationException
      let simulationDto = deserializedValue.data
      let nodeDto = SimulationDto simulationDto
      let nodeDtoData = {
        data = nodeDto
      }
      let! domainValue =
          nodeDtoData
          |> getNodeFromDtoData

      return domainValue
  }

module NodeDto = 
  // let fromDomain (domainObj:Node) :NodeDto =
  //   let nullBData = Nullable()
  //   let nullCData = null
  //   let nullDData = Unchecked.defaultof<NameDto>
  //   match domainObj with
  //   | A ->
  //     {Tag="A"; BData=nullBData; CData=nullCData; DData=nullDData}
  //   | B i ->
  //     let bdata = Nullable i
  //     {Tag="B"; BData=bdata; CData=nullCData; DData=nullDData}
  //   | C strList ->
  //     let cdata = strList |> List.toArray
  //     {Tag="C"; BData=nullBData; CData=cdata; DData=nullDData}
  //   | D name ->
  //     let ddata = name |> nameDtoFromDomain
  //     {Tag="D"; BData=nullBData; CData=nullCData; DData=ddata}
  let toDomain (dto: string * seq<string>) =
    let (jsonString, labels) = dto
    let labelArr = Seq.toArray labels
    printfn "toDomain labelArr:%A" labelArr
    let result: NodeOutput = 
      {
          Labels = labelArr
          Nodes = 
            Array.Parallel.map (
              function 
              | "File" -> 
                  jsonString 
                  |> FileDto.jsonToDomain
                  |> Some
              | "Simulation" -> 
                  jsonString 
                  |> SimulationDto.jsonToDomain
                  |> Some
              | _ -> None
            ) labelArr
      }
    result