module NetCDFInput
open System

type FirstOut = FirstOut of string
module FirstOut =
  let create fieldName str :Result<FirstOut, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (FirstOut str)
  let value d = match d with | FirstOut d -> d

type OutInterval = OutInterval of string
module OutInterval =
  let create fieldName str :Result<OutInterval, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (OutInterval str)
  let value d = match d with | OutInterval d -> d

type OutputStack = OutputStack of int
module OutputStack =
  let create fieldName n :Result<OutputStack, string> =
    if (n < 0) then
        Error (fieldName + " must not be negative")
    else
        Ok (OutputStack n)
  let value d = match d with | OutputStack d -> d