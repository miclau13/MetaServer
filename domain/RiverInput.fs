module RiverInput
open System

type InfoFile = InfoFile of string
module InfoFile =
  let create fieldName str :Result<InfoFile, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (InfoFile str)
  let value d = match d with | InfoFile d -> d

type Number = Number of int
module Number =
  let create fieldName n :Result<Number, string> =
    if (n < 0) then
        Error (fieldName + " must not be negative")
    else
        Ok (Number n)
  let value d = match d with | Number d -> d