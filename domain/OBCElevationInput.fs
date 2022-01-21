module OBCElevationInput
open System

type ElevationFile = ElevationFile of string
module ElevationFile =
  let create fieldName str :Result<ElevationFile, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (ElevationFile str)
  let value d = match d with | ElevationFile d -> d