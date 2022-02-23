module Command

open DbInstruction
open FileIOInstruction
open InterpretationProgram
open Logger
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
            | :? DbInstruction<Program<_>> as inst -> interpretDbInstruction loop inst
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
  
// For DB

// Get
let getAllNodesApi _ =
  Shell.getAllNodes ()
  |> interpret
  |> Async.RunSynchronously
let getAllRelationshipsApi _ =
  Shell.getAllRelationships ()
  |> interpret
  |> Async.RunSynchronously
let getNodesByChecksumApi checksum =
  Shell.getNodesByChecksum checksum
  |> interpret
  |> Async.RunSynchronously
let getNodesByLabelApi label =
  Shell.getNodesByLabel label
  |> interpret
  |> Async.RunSynchronously
let getPathsApi input =
  Shell.getPaths input
  |> interpret
  |> Async.RunSynchronously
let getPathsByNodeChecksumApi input =
  Shell.getPathsByNodeChecksum input
  |> interpret
  |> Async.RunSynchronously

// Create
let createNodesApi nodes =
  Shell.createNodes nodes
  |> interpret
  |> Async.RunSynchronously
let createNodesRelationshipApi nodes =
  Shell.createNodesRelationship nodes
  |> interpret
  |> Async.RunSynchronously

// Delete
let deleteAllNodesApi _ =
  Shell.deleteAllNodes ()
  |> interpret
  |> Async.RunSynchronously

// For FileIO
let copyDirectoryApi copyDirectoryApiInput =
  Shell.copyDirectory copyDirectoryApiInput
  |> interpret
  |> Async.RunSynchronously
let copyFileApi copyFileApiInput =
  Shell.copyFile copyFileApiInput
  |> interpret
  |> Async.RunSynchronously
let createDirectoryApi directoryPath =
  Shell.createDirectoryOnly directoryPath
  |> interpret
  |> Async.RunSynchronously
let createFileApi createFileApiInput =
  Shell.createFileOnly createFileApiInput
  |> interpret
  |> Async.RunSynchronously
let createSymbolicLinkApi createSymbolicLinkApiInput =
  Shell.createSymbolicLink createSymbolicLinkApiInput
  |> interpret
  |> Async.RunSynchronously
let updateFileApi updateFileApiInput =
  Shell.updateFileOnly updateFileApiInput
  |> interpret
  |> Async.RunSynchronously