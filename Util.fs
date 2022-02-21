module Util

open System.IO
open System.Text.RegularExpressions
open System.Security.Cryptography

type ResultBuilder() =
    member this.Return x = Ok x
    member this.Zero() = Ok ()
    member this.Bind(xResult,f) = Result.bind f xResult

let result = ResultBuilder()

let (|RegexGroup|_|) pattern (groupIndex: int) input  =
    let m = Regex.Match(input, pattern)
    if m.Success then Some m.Groups.[groupIndex].Value else None
let (|RegexTitle|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some input else None

let stringReplacementByRegex (pattern: string) (replacement: string) (input: string) =
    Regex.Replace(input, pattern, replacement)

let stringReplacement (originalStr: string) (replacementStr: string) (input: string) =
    input.Replace(originalStr, replacementStr)

let getChecksum (str: string) = 
    let bytes = 
        System.Text.Encoding.UTF8.GetBytes str
        |> SHA1.Create().ComputeHash
    let result = bytes |> Array.fold (fun acc b -> acc + b.ToString("X2")) ""
    result

let getChecksumStrFromChecksumArray (checksums: string []) = 
    let checksumStr = 
        checksums 
        |> Array.reduce (fun acc item -> 
            $"%s{acc}\n%s{item}"
        )
    checksumStr

let FileWithChecksumRegex = 
    sprintf "(\w{40}-)(.*)"

let getChecksumFileName (checksum: string) (fileName: string) = 
    match fileName with 
    | RegexGroup FileWithChecksumRegex 0 _  -> 
        fileName
    | _ ->  $"%s{checksum}-%s{fileName}"

// Modified on 12 Jan - Use one directory level only
let getChecksumDirFromChecksum (checksum: string) = 
    let directoryLevel1 = checksum.[0..1]
    directoryLevel1
   
let getFileNameAndFormat (file: string) = 
  let name = (file.Split [|'.'|]).[0]
  let format = (file.Split [|'.'|]).[1]
  (name, format)
  
// IO
let checkIfFileExist (path: string) =
    File.Exists path
let checkIfDirectoryExist  (path: string) =
    File.Exists path

let getFullPath (basePath: string, relativePath: string) =
    match basePath with 
    | "." | "" | "./" -> 
        let currentDirectory = Directory.GetCurrentDirectory()
        Path.Combine(currentDirectory, relativePath)
    | path -> 
        Path.Combine(path, relativePath)

let getChecksumFromFile (path: string) =
    let content = File.ReadAllBytes(path)
    let result = content |> SHA1.Create().ComputeHash |> Array.fold (fun acc b -> acc + b.ToString("X2")) ""
    result

let getAllFilesInDirectory (path: string) =
    let dir = DirectoryInfo(path)
    dir.GetFiles()