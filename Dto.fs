module Dto 

open Domain
open System

type Dto<'T> = {
  data: 'T
}

type GridDto = {
  Checksum: string
  NodeNumber: int
}

type FileDto = {
  Path: string
  Name: string
  Format: string
  Checksum: string
}

type FVCOMInputDto = {
  Checksum: string
  StartDate: string
  EndDate: string
}

type NodeDto = {
  Labels: string []
  GridData: Dto<GridDto>
  FileData: Dto<FileDto>
  FVCOMInputData: Dto<FVCOMInputDto>
}

/// Define a type to represent possible errors
type DtoError =
    | ValidationError of string
    | DeserializationException of exn

type NodeOutput = {
  Checksum: string
  Labels: string []
  Grid: Result<Grid, DtoError> option
  File: Result<File, DtoError> option
  FVCOMInput: Result<FVCOMInput, DtoError> option
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

module Grid =
  /// create a DTO from a domain object
  let fromDomain (grid: Domain.Grid) :GridDto =
    let checksum = grid.Checksum |> Checksum.value
    let nodeNumber = grid.NodeNumber |> NodeNumber.value
    let dto = { Checksum = checksum; NodeNumber = nodeNumber }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<GridDto>) :Result<Domain.Grid,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! nodeNumber = data.NodeNumber |> NodeNumber.create "NodeNumber"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        NodeNumber = nodeNumber
      }
    }

  // Serialize a grid into a JSON string
  let jsonFromDomain (grid: Domain.Grid) =
      grid
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a Grid
  let jsonToDomain jsonString :Result<Domain.Grid, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<GridDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module File =
  /// create a DTO from a domain object
  let fromDomain (file: Domain.File) :FileDto =
    let checksum = file.Checksum |> Checksum.value
    let name = file.Name |> Name.value
    let path = file.Path |> Path.value
    let format = file.Format |> Format.value
    let dto = { Checksum = checksum; Name = name; Path = path; Format = format } 
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<FileDto>) :Result<Domain.File,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! name = data.Name |> Name.create "Name"
      let! path = data.Path |> Path.create "Path"
      let! format = data.Format |> Format.create "Format"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        Name = name
        Path = path
        Format = format
      }
    }

  // Serialize a file into a JSON string
  let jsonFromDomain (file: Domain.File) =
      file
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a File
  let jsonToDomain jsonString :Result<Domain.File, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<FileDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module FVCOMInput =
  /// create a DTO from a domain object
  let fromDomain (fvcomInput: Domain.FVCOMInput) :FVCOMInputDto =
    let checksum = fvcomInput.Checksum |> Checksum.value
    let startDate = fvcomInput.StartDate |> StartDate.value
    let endDate = fvcomInput.EndDate |> EndDate.value
    let dto = { Checksum = checksum; StartDate = startDate; EndDate = endDate }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<FVCOMInputDto>) :Result<Domain.FVCOMInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! startDate = data.StartDate |> StartDate.create "StartDate"
      let! endDate = data.EndDate |> EndDate.create "EndDate"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        StartDate = startDate
        EndDate = endDate
      }
    }

  // Serialize a fvcomInput into a JSON string
  let jsonFromDomain (fvcomInput: Domain.FVCOMInput) =
      fvcomInput
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a FVCOMInput
  let jsonToDomain jsonString :Result<Domain.FVCOMInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<FVCOMInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

// //12/11/2021
// // Serialize a fvcomInput into a JSON string
// let jsonFromDomain (input: 'U, fromDomain: 'U -> Dto<'T>) =
//     input
//     |> fromDomain
//     |> Json.serialize

// /// Deserialize a JSON string into a FVCOMInput
// let jsonToDomain<'T> (jsonString: string) toDomain :Result<'U, DtoError> =
//   result {
//     let! deserializedValue =
//         jsonString
//         |> Json.deserialize<'T>
//         |> Result.mapError DeserializationException

//     let! domainValue =
//         deserializedValue
//         |> toDomain
//         |> Result.mapError ValidationError

//     return domainValue
// }

// let dtoToDomain<'T> (labels: string []) (label: string) (jsonString) (toDomain) = 
//   if Array.contains label labels then
//     jsonToDomain<'T> jsonString toDomain
//     |> Some
//   else None

module Node = 
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
    let result = 
      {
          Labels = labelArr
          File = 
            if Array.contains "File" labelArr then
              jsonString 
              |> File.jsonToDomain
              |> Some
            else None
          Grid = 
            if Array.contains "Grid" labelArr then
              jsonString 
              |> Grid.jsonToDomain
              |> Some
            else None
          FVCOMInput = 
            if Array.contains "FVCOMInput" labelArr then
              jsonString 
              |> FVCOMInput.jsonToDomain
              |> Some
            else None
          Checksum = ""
      }
    let checksum = 
        match result with 
        | { File = Some r } -> 
          match r with 
          | Ok value -> match value.Checksum with | Checksum v -> v
          | Error e -> e.ToString()
        | { Grid = Some r } -> 
          match r with 
          | Ok value -> match value.Checksum with | Checksum v -> v
          | Error e -> e.ToString()
        | { FVCOMInput = Some r } -> 
          match r with 
          | Ok value -> match value.Checksum with | Checksum v -> v
          | Error e -> e.ToString()
        | _ -> "No checksum"
    { result with Checksum = checksum }