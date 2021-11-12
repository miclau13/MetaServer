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

type NodeOutput = {
  Labels: string []
  Grid: Result<Grid, string> option
  File: Result<File, string> option
  FVCOMInput: Result<FVCOMInput, string> option
}

/// Define a type to represent possible errors
type DtoError =
    | ValidationError of string
    | DeserializationException of exn

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
  let fromDomain (grid: Domain.Grid) :Dto<GridDto> =
    let checksum = grid.Checksum |> Checksum.value
    let nodeNumber = grid.NodeNumber |> NodeNumber.value
    let dto = { Checksum = checksum; NodeNumber = nodeNumber } |> toDto
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
          |> Json.deserialize
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module File =
  /// create a DTO from a domain object
  let fromDomain (file: Domain.File) :Dto<FileDto> =
    let checksum = file.Checksum |> Checksum.value
    let name = file.Name |> Name.value
    let path = file.Path |> Path.value
    let format = file.Format |> Format.value
    let dto = { Checksum = checksum; Name = name; Path = path; Format = format } |> toDto
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
          |> Json.deserialize
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module FVCOMInput =
  /// create a DTO from a domain object
  let fromDomain (fvcomInput: Domain.FVCOMInput) :Dto<FVCOMInputDto> =
    let checksum = fvcomInput.Checksum |> Checksum.value
    let startDate = fvcomInput.StartDate |> StartDate.value
    let endDate = fvcomInput.EndDate |> EndDate.value
    let dto = { Checksum = checksum; StartDate = startDate; EndDate = endDate } |> toDto
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
          |> Json.deserialize
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
// let jsonToDomain<'T> (jsonString: string) :Result<'U, DtoError> =
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

module Node = 

  // 10/11/2021
  let getDataInDomainFormat<'T> (node: string) =
      Json.deserialize<'T> (node)
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
  let toDomain (dto: NodeDto) =
    let result = 
      {
          Labels = dto.Labels
          File = 
            match box dto.FileData with
            | null -> None
            | _ -> 
                dto.FileData
                |> File.toDomain
                |> Some
          Grid =
            match box dto.GridData with
              | null -> None
              | _ -> 
                  dto.GridData
                  |> Grid.toDomain
                  |> Some
          FVCOMInput =
            match box dto.FVCOMInputData with
              | null -> None
              | _ -> 
                  dto.FVCOMInputData
                  |> FVCOMInput.toDomain
                  |> Some 
      }
    result
    // match dto.FirstLabel with
    // | "File" ->
    //   match box dto.FileData with
    //   | null ->
    //       Error "File data not expected to be null"
    //   | _ ->
    //       dto.FileData
    //       |> File.toDomain  // returns Result...
    //       |> Result.map File     // ...so must use "map"
    // | "Grid" ->
    //   match box dto.GridData with
    //   | null ->
    //       Error "Grid data not expected to be null"
    //   | _ ->
    //       dto.GridData
    //       |> Grid.toDomain  // returns Result...
    //       |> Result.map Grid     // ...so must use "map"
    // | _ ->
    //   // all other cases
    //   let msg = sprintf "Label '%s' not recognized" dto.FirstLabel
    //   Error msg