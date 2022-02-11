module Command

open InterpretationProgram
open Logger
open FileIOInstruction
let interpret program =
  // 1. get the extra parameters and partially apply them to make all the interpreters
  // have a consistent shape
  //let smtpCredentials = defaultSmtpCredentials
  //let dbConnection = defaultDbService.NewDbConnection()
  //let interpretDbInstruction' = interpretDbInstruction dbConnection
  //let interpretEmailInstruction' = interpretEmailInstruction smtpCredentials

  // 2. define a recursive loop function. It has signature:
  //   AsyncResult<Program<'a>,InfrastructureError>) -> AsyncResult<'a,InfrastructureError>
  let rec loop programAS =
    asyncResult {
      let! program = programAS
      return!
        match program with
        | Instruction inst ->
            match inst with
            | :? LoggerInstruction<Program<_>> as inst -> interpretLogger loop inst
//            | :? DbInstruction<Program<_>> as inst -> interpretDbInstruction' loop inst
            | :? FileIOInstruction<Program<_>> as inst -> interpretFileIOInstruction loop inst
            | _ -> failwithf $"unknown instruction type {inst.GetType()}"
        | NotYetDone p ->
            loop (p() |> asyncResult.Return)
        | Stop value ->
            value |> asyncResult.Return
      }

  // 3. start the loop
  let initialProgram = program |> asyncResult.Return
  loop initialProgram

//let createTestFile (file: FileMeta) =
//  createFile file
//  |> interpret
//  |> Async.RunSynchronously
  
let copyTestFileApi (sourceFilePath, destFilePath) =
  FileIOInstruction.Shell.copyFile (sourceFilePath, destFilePath)
  |> interpret
  |> Async.RunSynchronously