module Util

// create an active pattern
open System.Text.RegularExpressions
open System.Security.Cryptography

let (|RegexGroup|_|) pattern (groupIndex: int) input  =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some m.Groups.[groupIndex].Value else None
let (|RegexTitle|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some input else None

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

let getChecksumInfoFromChecksumArray (checksums: string []) = 
    let checksumStr = 
        checksums 
        |> Array.reduce (fun acc item -> 
            sprintf "%s\n%s" acc item
        ) 
    let checksum = 
        checksumStr
        |> getChecksum
    (checksum, checksumStr)

let FileWithChecksumRegex = 
    sprintf "(\w{40}-)(.*)"

let getChecksumFileName (checksum: string) (fileName: string) = 
    match fileName with 
    | RegexGroup FileWithChecksumRegex 0 _  -> 
        fileName
    | _ ->  sprintf "%s-%s" checksum fileName

// Modified on 12 Jan - Use one directory level only
let getChecksumDirFromChecksum (checksum: string) = 
    let directoryLevel1 = checksum.[0..1]
    directoryLevel1

let getSimulationDirectoryPath (checksum: string, caseTitle: string, timestamp: string) = 
    sprintf "%s-%s-%s" checksum caseTitle timestamp