module Json

open FSharp.Json

let serialize obj =
  Json.serialize obj

let deserialize<'a> str =
  try
    Json.deserialize<'a> str
    |> Ok
  with
    // catch all exceptions and convert to Result
    | ex -> Error ex