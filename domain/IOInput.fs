module IOInput
open System

type InputDirectory = InputDirectory of string
module InputDirectory =
  let create fieldName str :Result<InputDirectory, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (InputDirectory str)
  let value d = match d with | InputDirectory d -> d

type OutputDirectory = OutputDirectory of string
module OutputDirectory =
  let create fieldName str :Result<OutputDirectory, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (OutputDirectory str)
  let value d = match d with | OutputDirectory d -> d