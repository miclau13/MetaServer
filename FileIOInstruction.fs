module FileIOInstruction

open Domain
open Instruction
open Logger
open InterpretationProgram
open System
open System.IO

type DestDirectoryPath = FullPath
type DestFileNamePrefix = string option
type DestFilePath = FullPath
type FileContent = string
type SourceDirectoryPath = FullPath
type SourceFilePath = FullPath

type CopyDirectoryInput = SourceDirectoryPath * DestDirectoryPath * DestFileNamePrefix
type CopyFileInput = SourceFilePath * DestFilePath
type CreateFileInput = DestFilePath * FileContent

type CreateSymbolicLinkInput = DestFilePath * SourceFilePath
type UpdateFileInput = DestFilePath * FileContent

type Decision =
  | NoAction
  | CopyDirectoryDecision of CopyDirectoryInput
  | CopyFileDecision of CopyFileInput
  | CreateDirectoryDecision of DestDirectoryPath
  | CreateFileDecision of CreateFileInput
  | CreateSymbolicLinkDecision of CreateSymbolicLinkInput
  | UpdateFileDecision of UpdateFileInput

type FileIOInstruction<'a> =
  | CopyDirectory of CopyDirectoryInput * next:(unit-> 'a)
  | CopyFile of CopyFileInput * next:(unit-> 'a)
  | CreateDirectory of DestDirectoryPath * next:(unit-> 'a)
  | CreateFile of CreateFileInput * next:(unit-> 'a)
  | CreateSymbolicLink of CreateSymbolicLinkInput * next:(unit-> 'a)
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
      | CreateSymbolicLink (createSymbolicLinkInput, next) ->
          CreateSymbolicLink (createSymbolicLinkInput, next >> f)
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
let createSymbolicLink createSymbolicLinkInput =
  Instruction (CreateSymbolicLink(createSymbolicLinkInput, Stop))
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
     let sourceDirPath, destDirPath, _ = copyDirectoryInput
     program {
        do! logInfo($"Copying Files from ({sourceDirPath}) to ({destDirPath})...")
        return CopyDirectoryDecision copyDirectoryInput
      }
  let copyFile (copyFileInput: CopyFileInput) =
    let sourceFilePath, destFilePath = copyFileInput
    let isFileExist = checkIfFileExist destFilePath
    if isFileExist then
      program {
       do! logInfo($"File with path ({destFilePath}) already exists, no need to copy file again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Copying File from ({sourceFilePath}) to ({destFilePath})...")
        return CopyFileDecision copyFileInput
      }
      
  let createDirectory (directoryPath: DestDirectoryPath) =
    let isDirectoryExist = checkIfDirectoryExist directoryPath
    if isDirectoryExist then
      program {
       do! logInfo($"Directory with path ({directoryPath}) already exists, no need to create directory again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Creating Directory ({directoryPath})...")
        return CreateDirectoryDecision directoryPath
      } 
  let createFile (createFileInput: CreateFileInput) =
    let destFilePath, _ = createFileInput
    let isFileExist = checkIfFileExist destFilePath
    if isFileExist then
      program {
       do! logInfo($"File with path ({destFilePath}) already exists, no need to create file again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Creating File ({destFilePath})...")
        return CreateFileDecision createFileInput
      }
  let createSymbolicLink (createSymbolicLinkInput: CreateSymbolicLinkInput) =
    let path, pathToTarget = createSymbolicLinkInput
    let isSymbolicLinkExist = checkIfFileExist path
    if isSymbolicLinkExist then
      program {
       do! logInfo($"Symbolic link ({path}) already exists, no need to create symbolic link again.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Creating symbolic link ({path} ------> ({pathToTarget}))...")
        return CreateSymbolicLinkDecision createSymbolicLinkInput
      }
  let updateFile (updateFileInput: UpdateFileInput) =
    let destFilePath, _ = updateFileInput
    let isFileExist = checkIfFileExist destFilePath
    if not <| isFileExist then
      program {
       do! logInfo($"File with path ({destFilePath}) does not exists, cannot update the file.")
       return NoAction
      }
    else
      program {
        do! logInfo($"Updating File ({destFilePath})...")
        return UpdateFileDecision updateFileInput
      } 

module Shell =
  let handleDecision (decision:Decision) :Program<unit> =
    match decision with
    | CopyDirectoryDecision copyDirectoryInput ->
        let _, destDirPath, _ = copyDirectoryInput
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
    | CreateSymbolicLinkDecision createSymbolicLinkInput ->
        createSymbolicLink createSymbolicLinkInput
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
  let createSymbolicLink createSymbolicLinkInput =
    program {
      let! decision = Pure.createSymbolicLink createSymbolicLinkInput
      do! handleDecision decision
    }
    
  let updateFileOnly updateFileInput =
    program {
      let! decision = Pure.updateFile updateFileInput
      do! handleDecision decision
    }

module Impure =
  let copyDirectory (copyDirectoryInput: CopyDirectoryInput) =
    let FullPath sourceDirPath, FullPath destDirPath, destFileNamePrefix = copyDirectoryInput
    try
       let srcDir = DirectoryInfo(sourceDirPath)
       for file in srcDir.GetFiles() do
          let fileName = file.Name
          let fileNameWithPrefix =
              match destFileNamePrefix with
                | Some v -> $"%s{v}-%s{fileName}"
                | None -> fileName
          let tempPath = Path.Combine(destDirPath, fileNameWithPrefix)
          if checkIfFileExist (FullPath tempPath) then
            Ok($"File with path (%s{tempPath}) already exists, no need to copy file again.")
            |> logResult
            |> ignore
          else
            file.CopyTo(tempPath) |> ignore
            Ok $"%s{sourceDirPath}/%s{fileName} --- copied to ---> %s{tempPath}"
            |> logResult
            |> ignore
       Ok $"Finish files in %s{sourceDirPath} --- copied to ---> %s{destDirPath}"
       |> logResult
    with
    | exn ->
      Error $"copyFile failed with exception{exn}"
      |> logResult
  let copyFile (copyFileInput: CopyFileInput) =
    let FullPath sourceFilePath, FullPath destFilePath = copyFileInput
    try
       File.Copy(sourceFilePath, destFilePath)
       Ok $"%s{sourceFilePath} --- copied to ---> %s{destFilePath}"
       |> logResult
    with
    | exn ->
      Error $"copyFile failed with exception{exn}"
      |> logResult
  let createDirectory (directoryPath: DestDirectoryPath) =
    let (FullPath dirPath) = directoryPath
    try
       let info = Directory.CreateDirectory dirPath
       Ok $"Directory is created with info: %A{info}"
       |> logResult
    with
    | exn ->
      Error $"copyFile failed with exception{exn}"
      |> logResult
  let createFile (createFileInput: CreateFileInput) =
    let FullPath destFilePath, content = createFileInput
    try
       let fs = File.Create destFilePath
       let info = Text.UTF8Encoding(true).GetBytes(content)
       fs.Write(info, 0, info.Length)
       fs.Close()
       Ok $"New file is created at %s{destFilePath}."
       |> logResult
    with
    | exn ->
      Error $"createFile failed with exception{exn}"
      |> logResult
      
  let createSymbolicLink (createSymbolicLinkInput: CreateSymbolicLinkInput) =
    let FullPath path, FullPath pathToTarget = createSymbolicLinkInput
    try
       let info = File.CreateSymbolicLink(path, pathToTarget)
       Ok $"Created Symbolic link %s{path}------> %s{pathToTarget} with info {info}."
       |> logResult
    with
    | exn ->
      Error $"createSymbolicLink failed with exception{exn}"
      |> logResult
  let updateFile (updateFileInput: UpdateFileInput) =
    let FullPath destFilePath, content = updateFileInput
    try
       let fs = File.AppendText destFilePath
       let info = $"%s{Environment.NewLine}%s{content}"
       fs.WriteLine(info)
       fs.Close()
       Ok $"File (%s{destFilePath}) is update with info (%s{info})."
       |> logResult
    with
    | exn ->
      Error $"UpdateFile failed with exception{exn}"
      |> logResult
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
  | CreateSymbolicLink (createSymbolicLinkInput, next) ->
      let unitAS = Impure.createSymbolicLink createSymbolicLinkInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | UpdateFile (updateFileInput, next) ->
      let unitAS = Impure.updateFile updateFileInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS