module Domain 
  
open System

type RelationShip = {
  Direction: Direction
  Node: string
}
and Direction = From | To

// Grid and File
type Checksum = Checksum of string
module Checksum =
  let create fieldName str :Result<Checksum,string> =
    if String.IsNullOrEmpty(str) then
        Error (fieldName + " must be non-empty")
    else
        Ok (Checksum str)

  let value checksum = match checksum with | Checksum c -> c

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

// FVCOMInput
type StartDate = StartDate of string
module StartDate =
  let create fieldName str :Result<StartDate, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (StartDate str)
  let value d = match d with | StartDate d -> d

// FVCOMInput
type EndDate = EndDate of string
module EndDate =
  let create fieldName str :Result<EndDate, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (EndDate str)
  let value d = match d with | EndDate d -> d

type Grid = {
  Checksum: Checksum
  NodeNumber: NodeNumber
  // RelationShip: RelationShip
}

type File = {
  Path: Path
  Name: Name
  Format: Format
  Checksum: Checksum
}

type FVCOMInput = {
  Checksum: Checksum
  StartDate: StartDate
  EndDate: EndDate
}

type Node = 
  | Grid of Grid
  | File of File
  | FVCOMInput of FVCOMInput

