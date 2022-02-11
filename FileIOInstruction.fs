module FileIOInstruction

open Instruction
open Logger
open InterpretationProgram
open System.IO

type SourceFilePath = string
type DestFilePath = string

type Decision =
  | NoAction
  | CopyFileOnly of SourceFilePath * DestFilePath
//  | UpdateProfileOnly of Profile
//  | UpdateProfileAndNotify of Profile * EmailMessage

//type DbInstruction<'a> =
//  | QueryProfile of UserId * next:(Profile -> 'a)
//  | UpdateProfile of Profile * next:(unit -> 'a)
//  interface IInstruction<'a> with
//    member this.Map f  =
//      match this with
//      | QueryProfile (userId,next) ->
//          QueryProfile (userId,next >> f)
//      | UpdateProfile (profile,next) ->
//          UpdateProfile (profile, next >> f)
//      :> IInstruction<_>

let checkIfFileExist (destFilePath: DestFilePath) =
  File.Exists destFilePath

type FileIOInstruction<'a> =
  | CopyFile of (SourceFilePath*DestFilePath) * next:(unit-> 'a)
//  | NoAction of next:(unit-> 'a)
//  | CreateDirectory of DirectoryInfo * next:(unit-> 'a)
//  | CreateFile of FileInfo * next:(unit-> 'a)
//  | DeleteFile of FileInfo * next:(unit-> 'a)
//  | DeleteDirectory of DirectoryInfo * next:(unit-> 'a)
//  | UpdateFile of FileInfo * next:(unit-> 'a)
  interface IInstruction<'a> with
    member this.Map f =
      match this with
      | CopyFile ((sourceFilePath, destFilePath),next) ->
          CopyFile ((sourceFilePath, destFilePath),next >> f)
//      | CreateDirectory (folderInfo,next) ->
//          CreateDirectory (folderInfo,next >> f)
//      | CreateFile (fileInfo,next) ->
//          CreateFile (fileInfo,next >> f)
//      | DeleteFile (fileInfo,next) ->
//          DeleteFile (fileInfo,next >> f)
//      | DeleteDirectory (folderInfo,next) ->
//          DeleteDirectory (folderInfo,next >> f)
//      | UpdateFile (fileInfo,next) ->
//          UpdateFile (fileInfo,next >> f)
      :> IInstruction<_>

let copyFile (sourceFilePath, destFilePath) =
  Instruction (CopyFile((sourceFilePath, destFilePath), Stop))
  
//let CreateDirectory folderInfo =
//  Instruction (CreateDirectory(folderInfo,Stop))
//let CreateFile fileInfo =
//  Instruction (CreateFile(fileInfo,Stop))
//
//let DeleteFile fileInfo =
//  Instruction (DeleteFile(fileInfo,Stop))
//let DeleteDirectory folderInfo =
//  Instruction (DeleteDirectory(folderInfo,Stop))
//
//let UpdateFile fileInfo =
//  Instruction (UpdateFile(fileInfo,Stop))
  
//let getProfile (userId:UserId) :Program<Profile> =
//  program {
//    return! queryProfile userId
//  }

//let checkIsFileExists (filePath: DestFilePath) =
//  program {
//    try
//      
//  }

module Pure =
  let copyFile (sourceFilePath: SourceFilePath, destFilePath: DestFilePath) =
    let isFileExist = checkIfFileExist destFilePath
    if isFileExist then
      program {
       do! logInfo($"File with path (%s{destFilePath}) already exists, no need to copy file again.")
       return NoAction
      }
    else
      program {
        do! logInfo("copying File...")
        return CopyFileOnly (sourceFilePath, destFilePath)
      } 

module Shell =
  let handleDecision (decision:Decision) :Program<unit> =
    match decision with
    | NoAction ->
        program.Zero()
    | CopyFileOnly (sourceFilePath, destFilePath) ->
        copyFile (sourceFilePath, destFilePath)
  let copyFile (sourceFilePath: SourceFilePath, destFilePath: DestFilePath) =
    program {
      let! decision = Pure.copyFile (sourceFilePath, destFilePath)
      do! handleDecision decision
    }

module Impure = 
  let copyFile (sourceFilePath: SourceFilePath, destFilePath: DestFilePath) =
    try
       File.Copy(sourceFilePath, destFilePath)
       globalLogger.Info $"%s{sourceFilePath} --- copied to ---> %s{destFilePath}"
       |> asyncResult.Return
    with
    | exn ->
      globalLogger.Error $"copyFile failed with exception{exn}"
      |> asyncResult.Return

//let createFile (file: FileMeta) =
//  program {
//    do! logInfo("creating File")
//    let currentDirectory = Directory.GetCurrentDirectory()
//    let { Name = FileName fileName } = file
//    let sourceFilePath = Path.Combine(currentDirectory, "input")
//    let destFilePath = Path.Combine(currentDirectory, fileName)
//    File.Copy(sourceFilePath, destFilePath)
//    do! logInfo("created File")
////    let! currentProfile = getProfile newProfile.UserId
////    let! decision = Pure.updateCustomerProfile newProfile currentProfile
//    let decision = NoAction
//    do! handleDecision decision
//  }
  
let interpretFileIOInstruction interpret (inst: FileIOInstruction<'a>) =
  match inst with
  | CopyFile ((sourceFilePath, destFilePath), next) ->
      let unitAS = Impure.copyFile (sourceFilePath, destFilePath)
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
