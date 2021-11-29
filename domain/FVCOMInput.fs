module FVCOMInput 
open System

type CaseTitle = CaseTitle of string
module CaseTitle =
  let create fieldName str :Result<CaseTitle, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (CaseTitle str)
  let value d = match d with | CaseTitle d -> d

type DateFormat = DateFormat of string
module DateFormat =
  let create fieldName str :Result<DateFormat, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (DateFormat str)
  let value d = match d with | DateFormat d -> d

type EndDate = EndDate of string
module EndDate =
  let create fieldName str :Result<EndDate, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (EndDate str)
  let value d = match d with | EndDate d -> d

type StartDate = StartDate of string
module StartDate =
  let create fieldName str :Result<StartDate, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (StartDate str)
  let value d = match d with | StartDate d -> d

type TimeZone = TimeZone of string
module TimeZone =
  let create fieldName str :Result<TimeZone, string> =
    if (String.IsNullOrEmpty(str)) then
        Error (fieldName + " must be non-empty")
    else
        Ok (TimeZone str)
  let value d = match d with | TimeZone d -> d