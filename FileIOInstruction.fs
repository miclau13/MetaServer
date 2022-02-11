module FileIOInstruction

open Instruction
open Logger
open InterpretationProgram
open System
open System.IO

type DestDirectoryPath = string
type DestFileNamePrefix = string option
type DestFilePath = string
type FileContent = string
type SourceDirectoryPath = string
type SourceFilePath = string

type CopyDirectoryInput = SourceDirectoryPath * DestDirectoryPath * DestFileNamePrefix
type CopyFileInput = SourceFilePath * DestFilePath
type CreateFileInput = DestFilePath * FileContent
type UpdateFileInput = DestFilePath * FileContent

type Decision =
  | NoAction
  | CopyDirectoryDecision of CopyDirectoryInput
  | CopyFileDecision of CopyFileInput
  | CreateDirectoryDecision of DestDirectoryPath
  | CreateFileDecision of CreateFileInput
  | UpdateFileDecision of UpdateFileInput

let checkIfFileExist (destFilePath: DestFilePath) =
    File.Exists destFilePath

let checkIfDirectoryExist (path: DestDirectoryPath) =
    Directory.Exists path

type FileIOInstruction<'a> =
  | CopyDirectory of CopyDirectoryInput * next:(unit-> 'a)
  | CopyFile of CopyFileInput * next:(unit-> 'a)
  | CreateDirectory of DestDirectoryPath * next:(unit-> 'a)
  | CreateFile of CreateFileInput * next:(unit-> 'a)
//  | DeleteFile of FileInfo * next:(unit-> 'a)
//  | DeleteDirectory of DirectoryInfo * next:(unit-> 'a)
  | UpdateFile of UpdateFileInput * next:(unit-> 'a)
  interface IInstruction<'a> with
    member this.Map f =
      match this with
      | CopyDirectory (copyDirectoryInput, next) ->
          CopyDirectory (copyDirectoryInput, next >> f)
      | CopyFile (copyFileInput, next) ->
          CopyFile (copyFileInput, next >> f)
      | CreateDirectory (directoryPath, next) ->
          CreateDirectory (directoryPath, next >> f)
      | CreateFile (createFileInput, next) ->
          CreateFile (createFileInput, next >> f)
//      | DeleteFile (fileInfo,next) ->
//          DeleteFile (fileInfo,next >> f)
//      | DeleteDirectory (folderInfo,next) ->
//          DeleteDirectory (folderInfo,next >> f)
      | UpdateFile (updateFileInput, next) ->
          UpdateFile (updateFileInput, next >> f)
      :> IInstruction<_>

let copyDirectory copyDirectoryInput =
  Instruction (CopyDirectory(copyDirectoryInput, Stop))
let copyFile copyFileInput =
  Instruction (CopyFile(copyFileInput, Stop))
let createDirectory directoryPath =
  Instruction (CreateDirectory(directoryPath, Stop))
let createFile createFileInput =
  Instruction (CreateFile(createFileInput, Stop))
//
//let DeleteFile fileInfo =
//  Instruction (DeleteFile(fileInfo,Stop))
//let DeleteDirectory folderInfo =
//  Instruction (DeleteDirectory(folderInfo,Stop))
//
let updateFile updateFileInput =
  Instruction (UpdateFile(updateFileInput,Stop))

module Pure =
  let copyDirectory (copyDirectoryInput: CopyDirectoryInput) =
     let (sourceDirPath, destDirPath, _) = copyDirectoryInput
     program {
        do! logInfo($"Copying Files from (%s{sourceDirPath}) to (%s{destDirPath})...")
        return CopyDirectoryDecision copyDirectoryInput
      }
  let copyFile (copyFileInput: CopyFileInput) =
    let (sourceFilePath, destFilePath) = copyFileInput
    let isFileExist = checkIfFileExist destFilePath
    if isFileExist then
      program {
       do! logInfo($"File with path (%s{destFilePath}) already exists, no need to copy file again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"copying File from (%s{sourceFilePath}) to (%s{destFilePath})...")
        return CopyFileDecision (sourceFilePath, destFilePath)
      }
      
  let createDirectory (directoryPath: DestDirectoryPath) =
    let isDirectoryExist = checkIfDirectoryExist directoryPath
    if isDirectoryExist then
      program {
       do! logInfo($"Directory with path (%s{directoryPath}) already exists, no need to create directory again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"creating Directory (%s{directoryPath})...")
        return CreateDirectoryDecision directoryPath
      } 
  let createFile (createFileInput: CreateFileInput) =
    let (destFilePath, content) = createFileInput
    let isFileExist = checkIfFileExist destFilePath
    if isFileExist then
      program {
       do! logInfo($"File with path (%s{destFilePath}) already exists, no need to create file again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"creating File (%s{destFilePath})...")
        return CreateFileDecision (destFilePath, content)
      }
  let updateFile (updateFileInput: UpdateFileInput) =
    let (destFilePath, content) = updateFileInput
    let isFileExist = checkIfFileExist destFilePath
    if not <| isFileExist then
      program {
       do! logInfo($"File with path (%s{destFilePath}) does not exists, cannot update the file.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Updating File (%s{destFilePath})...")
        return UpdateFileDecision (destFilePath, content)
      } 

module Shell =
  let handleDecision (decision:Decision) :Program<unit> =
    match decision with
    | CopyDirectoryDecision copyDirectoryInput ->
        let (_, destDirPath, _) = copyDirectoryInput
        program {
          do! createDirectory destDirPath
          do! copyDirectory copyDirectoryInput
        }
    | CopyFileDecision copyFileInput ->
        copyFile copyFileInput
    | CreateDirectoryDecision directoryPath ->
        createDirectory directoryPath
    | CreateFileDecision createFileInput ->
        createFile createFileInput
    | UpdateFileDecision updateFileInput ->
        updateFile updateFileInput
    | NoAction ->
        program.Zero()
  let copyDirectory copyDirectoryInput =
    program {
      let! decision = Pure.copyDirectory copyDirectoryInput
      do! handleDecision decision
    }
  let copyFile copyFileInput =
    program {
      let! decision = Pure.copyFile copyFileInput
      do! handleDecision decision
    }
  let createDirectoryOnly (directoryPath: DestDirectoryPath) =
    program {
      let! decision = Pure.createDirectory directoryPath
      do! handleDecision decision
    }
  let createFileOnly createFileInput =
    program {
      let! decision = Pure.createFile createFileInput
      do! handleDecision decision
    }
    
  let updateFileOnly updateFileInput =
    program {
      let! decision = Pure.updateFile updateFileInput
      do! handleDecision decision
    }

module Impure =
  let copyDirectory (copyDirectoryInput: CopyDirectoryInput) =
    let (sourceDirPath, destDirPath, destFileNamePrefix) = copyDirectoryInput
    try
       let srcDir = DirectoryInfo(sourceDirPath)
       for file in srcDir.GetFiles() do
          let fileName = file.Name
          let fileNameWithPrefix =
              match destFileNamePrefix with
                | Some v -> $"%s{v}-%s{fileName}"
                | None -> fileName
          let tempPath = Path.Combine(destDirPath, fileNameWithPrefix)
          if checkIfFileExist tempPath then
            globalLogger.Info($"File with path (%s{tempPath}) already exists, no need to copy file again.")
          else
            file.CopyTo(tempPath) |> ignore
            globalLogger.Info $"%s{sourceDirPath}/%s{fileName} --- copied to ---> %s{tempPath}"
       globalLogger.Info $"Finish files in %s{sourceDirPath} --- copied to ---> %s{destDirPath}"
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"copyFile failed with exception{exn}"
      |> asyncResult.Return
  let copyFile (copyFileInput: CopyFileInput) =
    let (sourceFilePath, destFilePath) = copyFileInput
    try
       File.Copy(sourceFilePath, destFilePath)
       globalLogger.Info $"%s{sourceFilePath} --- copied to ---> %s{destFilePath}"
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"copyFile failed with exception{exn}"
      |> asyncResult.Return
  let createDirectory (directoryPath: DestDirectoryPath) =
    try
       let info = Directory.CreateDirectory directoryPath
       globalLogger.Info $"Directory is created with info: %A{info}"
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"copyFile failed with exception{exn}"
      |> asyncResult.Return
  let createFile (createFileInput: CreateFileInput) =
    let (destFilePath, content) = createFileInput
    try
       let fs = File.Create destFilePath
       let info = Text.UTF8Encoding(true).GetBytes(content)
       fs.Write(info, 0, info.Length)
       fs.Close()
       globalLogger.Info $"New file is created at %s{destFilePath}."
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"createFile failed with exception{exn}"
      |> asyncResult.Return
  
  let updateFile (updateFileInput: UpdateFileInput) =
    let (destFilePath, content) = updateFileInput
    try
       let fs = File.AppendText destFilePath
       let info = $"%s{Environment.NewLine}%s{content}"
       fs.WriteLine(info)
       fs.Close()
       globalLogger.Info $"File (%s{destFilePath}) is update with info (%s{info})."
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"UpdateFile failed with exception{exn}"
      |> asyncResult.Return
let interpretFileIOInstruction interpret (inst: FileIOInstruction<'a>) =
  match inst with
  | CopyDirectory (copyDirectoryInput, next) ->
      let unitAS = Impure.copyDirectory copyDirectoryInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | CopyFile (copyFileInput, next) ->
      let unitAS = Impure.copyFile copyFileInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | CreateDirectory (directoryPath, next) ->
      let unitAS = Impure.createDirectory directoryPath
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | CreateFile (createFileInput, next) ->
      let unitAS = Impure.createFile createFileInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | UpdateFile (updateFileInput, next) ->
      let unitAS = Impure.updateFile updateFileInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS