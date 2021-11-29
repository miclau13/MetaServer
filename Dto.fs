module Dto 

open Domain
open System
open FVCOMInput
open GridCoordinatesInput
open IOInput
open NetCDFInput
open RiverInput
open OBCInput

type Dto<'T> = {
  data: 'T
}

// Input Dto from config
type AirPressureInputDto = {
  Checksum: string
  File: string
  Kind: string
}
type FVCOMInputDto = {
  CaseTitle: String
  Checksum: string
  DateFormat: string
  EndDate: string
  StartDate: string
  TimeZone: string
}
type GridCoordinatesInputDto = {
  Checksum: string
  File: string
  FileUnits: string
}
type HeatingInputDto = {
  Checksum: string
  File: string
  Type: string
}
type IOInputDto = {
  Checksum: string
  InputDirectory: string
  OutputDirectory: string
}
type NetCDFInputDto = {
  Checksum: string
  FirstOut: string
  OutInterval: string
  OutputStack: int
}
type OBCInputDto = {
  Checksum: string
  NodeListFile: string
  ElevationFile: string
}
type RiverInputDto = {
  Checksum: string
  InfoFile: string
  Number: int
  Kind: string
}
type StartupInputDto = {
  Checksum: string
  File: string
  Type: string
}
type StartupXInputDto = {
  Checksum: string
  File: string
  Type: string
}
type WaveInputDto = {
  Checksum: string
  File: string
  Kind: string
}
type WindInputDto = {
  Checksum: string
  File: string
  Type: string
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

type NodeDto = {
  Labels: string []
  GridData: Dto<GridDto>
  FileData: Dto<FileDto>
  FVCOMInputData: Dto<FVCOMInputDto>
  StartupInputData: Dto<StartupInputDto>
}

type HasInputDTO = {
  SID: string
}

type FileLocationIsDTO = {
  BasicPath: string
}
type RelationshipDto = 
  | HasInputDTO of HasInputDTO
  | FileLocationIsDTO of FileLocationIsDTO

/// Define a type to represent possible errors
type DtoError =
    | ValidationError of string
    | DeserializationException of exn

type NodeOutput = {
  Checksum: string
  Labels: string []
  Grid: Result<Grid, DtoError> option
  File: Result<File, DtoError> option
  AirPressureInput: Result<AirPressureInput, DtoError> option
  FVCOMInput: Result<FVCOMInput, DtoError> option
  GridCoordinatesInput: Result<GridCoordinatesInput, DtoError> option
  HeatingInput: Result<HeatingInput, DtoError> option
  IOInput: Result<IOInput, DtoError> option
  NetCDFInput: Result<NetCDFInput, DtoError> option
  OBCInput: Result<OBCInput, DtoError> option
  RiverInput: Result<RiverInput, DtoError> option
  StartupInput: Result<StartupInput, DtoError> option
  StartupXInput: Result<StartupXInput, DtoError> option
  WaveInput: Result<WaveInput, DtoError> option
  WindInput: Result<WindInput, DtoError> option
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


module AirPressureInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.AirPressureInput) :AirPressureInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let kind = input.Kind |> InputKind.value
    let dto: AirPressureInputDto = {
      Checksum = checksum
      File = file
      Kind = kind
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<AirPressureInputDto>) :Result<Domain.AirPressureInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! kind = data.Kind |> InputKind.create "Kind"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Kind = kind
      }
    }

  // Serialize the AirPressureInput into a JSON string
  let jsonFromDomain (input: Domain.AirPressureInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a AirPressureInput
  let jsonToDomain jsonString :Result<Domain.AirPressureInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<AirPressureInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }
module FVCOMInputDto =
  /// create a DTO from a domain object
  let fromDomain (fvcomInput: Domain.FVCOMInput) :FVCOMInputDto =
    let checksum = fvcomInput.Checksum |> Checksum.value
    let startDate = fvcomInput.StartDate |> StartDate.value
    let endDate = fvcomInput.EndDate |> EndDate.value
    let caseTitle = fvcomInput.CaseTitle |> CaseTitle.value
    let timezone = fvcomInput.TimeZone |> TimeZone.value
    let dateFormat = fvcomInput.DateFormat |> DateFormat.value
    let dto = { 
      Checksum = checksum
      CaseTitle = caseTitle
      DateFormat = dateFormat
      EndDate = endDate
      StartDate = startDate
      TimeZone = timezone
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<FVCOMInputDto>) :Result<Domain.FVCOMInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! startDate = data.StartDate |> StartDate.create "StartDate"
      let! endDate = data.EndDate |> EndDate.create "EndDate"
      let! caseTitle = data.CaseTitle |> CaseTitle.create "CaseTitle"
      let! timezone = data.TimeZone |> TimeZone.create "TimeZone"
      let! dateFormat = data.DateFormat |> DateFormat.create "DateFormat"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        CaseTitle = caseTitle
        DateFormat = dateFormat
        EndDate = endDate
        StartDate = startDate
        TimeZone = timezone
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


module GridCoordinatesInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.GridCoordinatesInput) :GridCoordinatesInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let fileUnits = input.FileUnits |> FileUnits.value
    let dto: GridCoordinatesInputDto = {
      Checksum = checksum
      File = file
      FileUnits = fileUnits
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<GridCoordinatesInputDto>) :Result<Domain.GridCoordinatesInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! fileUnits = data.FileUnits |> FileUnits.create "FileUnits"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        FileUnits = fileUnits
      }
    }

  // Serialize the GridCoordinatesInput into a JSON string
  let jsonFromDomain (input: Domain.GridCoordinatesInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a GridCoordinatesInput
  let jsonToDomain jsonString :Result<Domain.GridCoordinatesInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<GridCoordinatesInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module HeatingInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.HeatingInput) :HeatingInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let inputType = input.Type |> InputType.value
    let dto: HeatingInputDto = {
      Checksum = checksum
      File = file
      Type = inputType
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<HeatingInputDto>) :Result<Domain.HeatingInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! inputType = data.Type |> InputType.create "Type"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Type = inputType
      }
    }

  // Serialize the HeatingInput into a JSON string
  let jsonFromDomain (input: Domain.HeatingInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a HeatingInput
  let jsonToDomain jsonString :Result<Domain.HeatingInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<HeatingInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module IOInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.IOInput) :IOInputDto =
    let checksum = input.Checksum |> Checksum.value
    let inputDirectory = input.InputDirectory |> InputDirectory.value
    let outputDirectory = input.OutputDirectory |> OutputDirectory.value
    let dto: IOInputDto = {
      Checksum = checksum
      InputDirectory = inputDirectory
      OutputDirectory = outputDirectory
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<IOInputDto>) :Result<Domain.IOInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! inputDirectory = data.InputDirectory |> InputDirectory.create "InputDirectory"
      let! outputDirectory = data.OutputDirectory |> OutputDirectory.create "OutputDirectory"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        InputDirectory = inputDirectory
        OutputDirectory = outputDirectory
      }
    }

  // Serialize the IOInput into a JSON string
  let jsonFromDomain (input: Domain.IOInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a IOInput
  let jsonToDomain jsonString :Result<Domain.IOInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<IOInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }


module NetCDFInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.NetCDFInput) :NetCDFInputDto =
    let checksum = input.Checksum |> Checksum.value
    let firstOut = input.FirstOut |> FirstOut.value
    let outInterval = input.OutInterval |> OutInterval.value
    let outputStack = input.OutputStack |> OutputStack.value
    let dto: NetCDFInputDto = {
      Checksum = checksum
      FirstOut = firstOut
      OutInterval = outInterval
      OutputStack = outputStack
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<NetCDFInputDto>) :Result<Domain.NetCDFInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! firstOut = data.FirstOut |> FirstOut.create "FirstOut"
      let! outInterval = data.OutInterval |> OutInterval.create "OutInterval"
      let! outputStack = data.OutputStack |> OutputStack.create "OutInterval"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        FirstOut = firstOut
        OutInterval = outInterval
        OutputStack = outputStack
      }
    }

  // Serialize the NetCDF into a JSON string
  let jsonFromDomain (input: Domain.NetCDFInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a NetCDF
  let jsonToDomain jsonString :Result<Domain.NetCDFInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<NetCDFInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module OBCInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.OBCInput) :OBCInputDto =
    let checksum = input.Checksum |> Checksum.value
    let elevationFile = input.ElevationFile |> ElevationFile.value
    let nodeListFile = input.NodeListFile |> NodeListFile.value
    let dto: OBCInputDto = {
      Checksum = checksum
      ElevationFile = elevationFile
      NodeListFile = nodeListFile
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<OBCInputDto>) :Result<Domain.OBCInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! elevationFile = data.ElevationFile |> ElevationFile.create "ElevationFile"
      let! nodeListFile = data.NodeListFile |> NodeListFile.create "NodeListFile"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        ElevationFile = elevationFile
        NodeListFile = nodeListFile
      }
    }

  // Serialize the OBCInput into a JSON string
  let jsonFromDomain (input: Domain.OBCInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a OBCInput
  let jsonToDomain jsonString :Result<Domain.OBCInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<OBCInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module RiverInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.RiverInput) :RiverInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.InfoFile |> InfoFile.value
    let number = input.Number |> Number.value
    let inputKind = input.Kind |> InputKind.value
    let dto: RiverInputDto = {
      Checksum = checksum
      InfoFile = file
      Number = number
      Kind = inputKind
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<RiverInputDto>) :Result<Domain.RiverInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.InfoFile |> InfoFile.create "InfoFile"
      let! number = data.Number |> Number.create "Number"
      let! inputKind = data.Kind |> InputKind.create "Kind"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        InfoFile = file
        Number = number
        Kind = inputKind
      }
    }

  // Serialize the RiverInput into a JSON string
  let jsonFromDomain (input: Domain.RiverInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a RiverInput
  let jsonToDomain jsonString :Result<Domain.RiverInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<RiverInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module StartupInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.StartupInput) :StartupInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let inputType = input.Type |> InputType.value
    let dto: StartupInputDto = {
      Checksum = checksum
      File = file
      Type = inputType
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<StartupInputDto>) :Result<Domain.StartupInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! inputType = data.Type |> InputType.create "Type"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Type = inputType
      }
    }

  // Serialize the StartupInput into a JSON string
  let jsonFromDomain (input: Domain.StartupInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a StartupInput
  let jsonToDomain jsonString :Result<Domain.StartupInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<StartupInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module StartupXInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.StartupXInput) :StartupXInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let inputType = input.Type |> InputType.value
    let dto: StartupXInputDto = {
      Checksum = checksum
      File = file
      Type = inputType
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<StartupXInputDto>) :Result<Domain.StartupXInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! inputType = data.Type |> InputType.create "Type"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Type = inputType
      }
    }

  // Serialize the StartupXInput into a JSON string
  let jsonFromDomain (input: Domain.StartupXInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a StartupXInput
  let jsonToDomain jsonString :Result<Domain.StartupXInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<StartupXInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module WaveInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.WaveInput) :WaveInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let startupKind = input.Kind |> InputKind.value
    let dto: WaveInputDto = {
      Checksum = checksum
      File = file
      Kind = startupKind
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<WaveInputDto>) :Result<Domain.WaveInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! startupKind = data.Kind |> InputKind.create "Kind"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Kind = startupKind
      }
    }

  // Serialize the WaveInput into a JSON string
  let jsonFromDomain (input: Domain.WaveInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a WaveInput
  let jsonToDomain jsonString :Result<Domain.WaveInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<WaveInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module WindInputDto =
  /// create a DTO from a domain object
  let fromDomain (input: Domain.WindInput) :WindInputDto =
    let checksum = input.Checksum |> Checksum.value
    let file = input.File |> InputFile.value
    let inputType = input.Type |> InputType.value
    let dto: WindInputDto = {
      Checksum = checksum
      File = file
      Type = inputType
    }
    dto

  /// create a domain object from a DTO
  let toDomain (dto: Dto<WindInputDto>) :Result<Domain.WindInput,string> =
    let data = dto |> fromDto
    result {
      // get each (validated) simple type from the DTO as a success or failure
      let! checksum = data.Checksum |> Checksum.create "Checksum"
      let! file = data.File |> InputFile.create "File"
      let! inputType = data.Type |> InputType.create "Type"
      // combine the components to create the domain object
      return {
        Checksum = checksum
        File = file
        Type = inputType
      }
    }

  // Serialize the WindInput into a JSON string
  let jsonFromDomain (input: Domain.WindInput) =
      input
      |> fromDomain
      |> Json.serialize

  /// Deserialize a JSON string into a WindInput
  let jsonToDomain jsonString :Result<Domain.WindInput, DtoError> =
    result {
      let! deserializedValue =
          jsonString
          |> Json.deserialize<Dto<WindInputDto>>
          |> Result.mapError DeserializationException

      let! domainValue =
          deserializedValue
          |> toDomain
          |> Result.mapError ValidationError

      return domainValue
  }

module GridDto =
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

module FileDto =
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
    let result = 
      {
          Labels = labelArr
          AirPressureInput = 
            if Array.contains "AirPressureInput" labelArr then
              jsonString 
              |> AirPressureInputDto.jsonToDomain
              |> Some
            else None
          FVCOMInput = 
            if Array.contains "FVCOMInput" labelArr then
              jsonString 
              |> FVCOMInputDto.jsonToDomain
              |> Some
            else None
          GridCoordinatesInput = 
            if Array.contains "GridCoordinatesInput" labelArr then
              jsonString 
              |> GridCoordinatesInputDto.jsonToDomain
              |> Some
            else None
          HeatingInput = 
            if Array.contains "HeatingInput" labelArr then
              jsonString 
              |> HeatingInputDto.jsonToDomain
              |> Some
            else None
          IOInput = 
            if Array.contains "IOInput" labelArr then
              jsonString 
              |> IOInputDto.jsonToDomain
              |> Some
            else None
          NetCDFInput = 
            if Array.contains "NetCDFInput" labelArr then
              jsonString 
              |> NetCDFInputDto.jsonToDomain
              |> Some
            else None
          OBCInput = 
            if Array.contains "OBCInput" labelArr then
              jsonString 
              |> OBCInputDto.jsonToDomain
              |> Some
            else None
          RiverInput = 
            if Array.contains "RiverInput" labelArr then
              jsonString 
              |> RiverInputDto.jsonToDomain
              |> Some
            else None
          StartupInput = 
            if Array.contains "StartupInput" labelArr then
              jsonString 
              |> StartupInputDto.jsonToDomain
              |> Some
            else None
          StartupXInput = 
            if Array.contains "StartupXInput" labelArr then
              jsonString 
              |> StartupXInputDto.jsonToDomain
              |> Some
            else None
          WaveInput = 
            if Array.contains "WaveInput" labelArr then
              jsonString 
              |> WaveInputDto.jsonToDomain
              |> Some
            else None
          WindInput = 
            if Array.contains "WindInput" labelArr then
              jsonString 
              |> WindInputDto.jsonToDomain
              |> Some
            else None
          File = 
            if Array.contains "File" labelArr then
              jsonString 
              |> FileDto.jsonToDomain
              |> Some
            else None
          Grid = 
            if Array.contains "Grid" labelArr then
              jsonString 
              |> GridDto.jsonToDomain
              |> Some
            else None
          Checksum = ""
      }
    let checksum = 
        match result with 
        | { StartupInput = Some r } -> 
          match r with 
          | Ok value -> match value.Checksum with | Checksum v -> v
          | Error e -> e.ToString()
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