module Logger

open Instruction
open InterpretationProgram

type LoggerInstruction<'a> =
  | LogInfo of string  * next:(unit -> 'a)
  | LogError of string * next:(unit -> 'a)
  interface IInstruction<'a> with
    member this.Map f  =
      match this with
      | LogInfo (str,next) ->
          LogInfo (str,next >> f)
      | LogError (str,next) ->
          LogError (str,next >> f)
      :> IInstruction<_>

type ILogger =
  abstract Debug : string -> unit
  abstract Info : string -> unit
  abstract Error : string -> unit

type DefaultLogger() =
   interface ILogger with
      member this.Debug(str: string) = printfn $"LOGGER DEBUG %s{str}"
      member this.Info(str: string) = printfn $"LOGGER INFO %s{str}"
      member this.Error(str: string) = printfn $"LOGGER ERROR %s{str}"

// helpers to use within the computation expression
let logInfo str = Instruction (LogInfo (str,Stop))
let logError str = Instruction (LogError (str,Stop))
let globalLogger = DefaultLogger() :> ILogger

let interpretLogger interpret inst =
  match inst with
  | LogInfo (str, next) ->
      globalLogger.Info str
      let newProgramAS = next() |> asyncResult.Return
      interpret newProgramAS
  | LogError (str, next) ->
      globalLogger.Error str
      let newProgramAS = next() |> asyncResult.Return
      interpret newProgramAS

let logResult result =
    match result with
    | Ok message -> 
        globalLogger.Info message
        |> asyncResult.Return
    | Error message -> 
        globalLogger.Error message
        |> asyncResult.Return