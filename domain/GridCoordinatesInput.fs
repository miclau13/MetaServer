module GridCoordinatesInput
open System

// type File = File of string
// module File =
//   let create fieldName str :Result<File, string> =
//     if (String.IsNullOrEmpty(str)) then
//         Error (fieldName + " must be non-empty")
//     else
//         Ok (File str)
//   let value d = match d with | File d -> d

type FileUnits = FileUnits of string
module FileUnits =
  let create fieldName str :Result<FileUnits, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (FileUnits str)
  let value d = match d with | FileUnits d -> d