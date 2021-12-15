module FileIO

open System
open System.IO
open Domain

let getChecksumFileName (checksum: string) (fileName: string) = 
    sprintf "%s-%s" checksum fileName

let getFullPathWithBasePath (basePath: string) (path: string) =
    match basePath with 
    | "." | "" | "./" -> 
        let currentDirectory = Directory.GetCurrentDirectory()
        sprintf "%s%s" currentDirectory path
    | p -> sprintf "%s%s" p path

let checkIfFileExist path =
    File.Exists path

let checkIfDirectoryExist path =
    Directory.Exists path

let createDirectoryIfNotExist path = 
    if (not <| checkIfDirectoryExist path) then
        let info = Directory.CreateDirectory path
        printfn "Directory Created with info: %A" info
    else  
        printfn "Directory(%A) exists." path

// Compress and copy the file 
let compressAndCopyFile originalFileName targetFileName = 
    // TODO Check why the file size below 1.3 MB can't be compressed
    let minimumFileSize = int64 1300000
    let originalFileStream =  (FileInfo originalFileName).OpenRead() 
    let originalFileStreamLength = originalFileStream.Length
    if originalFileStreamLength > minimumFileSize then
        try 
            let targetFileStream = (FileInfo targetFileName).Create()
            let compressor = new Compression.DeflateStream(targetFileStream, Compression.CompressionLevel.Optimal)
            originalFileStream.CopyTo(compressor)
            printfn "%s (Size: %i) --- compressed to ---> %s (Size: %i)" originalFileName originalFileStreamLength targetFileName targetFileStream.Length
            targetFileStream.Close()
        with 
            ex -> 
                printfn "%s" ex.Message
    originalFileStream.Close() 

// DCeompress the file 
let decompressFile originalFileName targetFileName = 
    if File.Exists originalFileName then
        try
            let compressedFileStream = File.Open(originalFileName, FileMode.Open);
            let outputFileStream = File.Create(targetFileName);
            let decompressor = new Compression.DeflateStream(compressedFileStream, Compression.CompressionMode.Decompress);
            decompressor.CopyTo(outputFileStream);
            printfn "%s (Size: %i) --- decompressed to ---> %s (Size: %i)" originalFileName compressedFileStream.Length targetFileName outputFileStream.Length
            outputFileStream.Close()
            compressedFileStream.Close()
        with 
            ex -> 
                printfn "%s" ex.Message
    else
        printfn "Source File does not exist."

let getPathInfoFromChecksum (checksum: string) = 
    let directoryLevel1 = checksum.[0..1]
    let directoryLevel2 = checksum.[2..3]
    let fileName = checksum.[4..]
    let path = sprintf "/%s/%s/" directoryLevel1 directoryLevel2
    (path, directoryLevel1, directoryLevel2, fileName)

let copyFile (sourceBasePath, sourceDirectory) (targetBasePath, targetDirectory) (file: File) =
    let targetDirectoryWithBasePath = getFullPathWithBasePath targetBasePath targetDirectory
    // Create the outer directory
    createDirectoryIfNotExist targetDirectoryWithBasePath
    // Create the full directory 
    let (Checksum checksum) = file.Checksum
    let (checksumDirectory, _, _, checksumFileName) = getPathInfoFromChecksum checksum
    let targetWithBasePath = getFullPathWithBasePath targetDirectoryWithBasePath checksumDirectory
    createDirectoryIfNotExist targetWithBasePath
    
    let sourceDirectoryWithBasePath = getFullPathWithBasePath sourceBasePath sourceDirectory
    let (Name fileName) = file.Name
    let (Format fileFormat) = file.Format
    let sourceFileName = sprintf "%s.%s" fileName fileFormat
    let targetFileName = getChecksumFileName checksumFileName fileName
    let sourcePath = Path.Combine(sourceDirectoryWithBasePath, sourceFileName)
    let targetPath = Path.Combine(targetWithBasePath, targetFileName)
    if not <| checkIfFileExist targetPath then
        File.Copy(sourcePath, targetPath)
        printfn "%s --- copied to ---> %s" sourcePath targetPath
    else
        printfn "targetPath (%s) already exists."  targetPath
    // compressFile targetPath (targetPath+".gz")
    // decompressFile (targetPath+".gz") (targetPath+".ungz")
    sprintf "%s-%s" checksum fileName

let createTreeFile (basePath, targetDirectory) (commitChecksums: string []) = 
    let currentTimeStamp = DateTime.UtcNow.ToString()
    let currentUser = Environment.UserName

    let checksumStr = 
        commitChecksums 
        |> Array.reduce (fun acc item -> 
            sprintf "%s\n%s" acc item
        ) 
    let checksum = 
        checksumStr
        |> Input.getChecksum
    
    let targetDirectoryWithBasePath = getFullPathWithBasePath basePath targetDirectory
    createDirectoryIfNotExist targetDirectoryWithBasePath
    let (checksumDirectory, _, _, checksumFileName) = getPathInfoFromChecksum checksum
    let targetFileName = getChecksumFileName checksumFileName "tree"
    let targetWithBasePath = getFullPathWithBasePath targetDirectoryWithBasePath checksumDirectory
    let targetPath = Path.Combine(targetWithBasePath, targetFileName)
    let commit = getChecksumFileName checksum "tree"
    let treeContent = 
        sprintf "commit %s\nAuthor: %s\nDate: %s\nRelated files: \n%s" commit currentUser currentTimeStamp checksumStr
    
    if not <| checkIfFileExist targetPath then
        try
            createDirectoryIfNotExist targetWithBasePath
            let fs = IO.File.Create targetPath
            let info = Text.UTF8Encoding(true).GetBytes(treeContent)
            fs.Write(info, 0, info.Length)
            printfn "New Tree Content Created." 
            fs.Close()
        with 
        ex -> 
            printfn "Tree Content No Created with Error: %s" ex.Message
    else 
        printfn "TargetPath (%s) already exists" targetPath