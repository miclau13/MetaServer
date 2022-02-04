module FileIO

open System
open System.IO
open System.Security.Cryptography
open Domain
open Util

type PathInfo = {
    BasePath: string
    RelativePath: string
}

type ChecksumFileInfo = {
    FileName: string
    Checksum: string
}

type ChecksumFilePathInfo = {
    FileFullPath: string
    FileDirFullPath: string
}

// Start of Get

let getCalDirectory = "cal"
let getTargetOutputDirectory = "output"

let checkIfFileExist (path: string) =
    File.Exists path

let checkIfDirectoryExist (path: string) =
    Directory.Exists path

let getAllFilesInDirectory (path: string) =
    if not <| Directory.Exists(path) then
        let msg = String.Format("Source directory does not exist or could not be found: {0}", path)
        raise (DirectoryNotFoundException(msg))

    let dir = DirectoryInfo(path)
    dir.GetFiles()

let getChecksumFromFile (path: string) =
    if IO.File.Exists(path) then
        if (FileInfo(path)).Length <> 0L then
            let content = IO.File.ReadAllBytes(path)
            let result = content |> SHA1.Create().ComputeHash |> Array.fold (fun acc b -> acc + b.ToString("X2")) ""
            result
        else
        $"File %s{path} has null length."
    else
        $"File %s{path} does not exist."

let getFullPath (fullPathInfo: PathInfo) =
    let { BasePath = basePath ; RelativePath = relativePath } = fullPathInfo
    match basePath with 
    | "." | "" | "./" -> 
        let currentDirectory = Directory.GetCurrentDirectory()
        Path.Combine(currentDirectory, relativePath)
    | path -> 
        Path.Combine(path, relativePath)

let getFilePathInfo (directoryFullPathInfo: PathInfo) (checksumFileInfo: ChecksumFileInfo) = 
    let { FileName = fileName; Checksum = checksum } = checksumFileInfo
    let checksumDirectory = getChecksumDirFromChecksum checksum
    let fileNameWithChecksum = getChecksumFileName checksum fileName
    let directoryFullPath = getFullPath directoryFullPathInfo

    // Get path with checksum Dir
    let directoryWithChecksumFullPath = Path.Combine(directoryFullPath, checksumDirectory)
    let fileFullPath = Path.Combine(directoryWithChecksumFullPath, fileNameWithChecksum)
    { FileFullPath = fileFullPath; FileDirFullPath = directoryWithChecksumFullPath }

let getChecksumFilePathInfo (directoryFullPathInfo: PathInfo) (file: File) =
    
    let (Checksum checksum) = file.Checksum
    let fileName = getFileName file
    let checksumFileInfo = { FileName = fileName ; Checksum = checksum }
    let targetFileName = getChecksumFileName checksum fileName

    let { FileFullPath = fileFullPath; FileDirFullPath = directoryWithChecksumFullPath } = getFilePathInfo directoryFullPathInfo checksumFileInfo
    fileFullPath, directoryWithChecksumFullPath, targetFileName

let getCalDirectoryFullPath (basePath: string) = 
    let calDirectory = getCalDirectory
    let fullPathInfo = { BasePath = basePath; RelativePath = calDirectory }
    let calDirectoryFullPath = getFullPath fullPathInfo
    calDirectoryFullPath

let isInputFileExisted (fullPathInfo: PathInfo) (file: File) =
    let targetDirectoryWithBasePath = getFullPath fullPathInfo

    let (Checksum checksum) = file.Checksum
    let checksumDirectory = getChecksumDirFromChecksum checksum
    let targetWithBasePath = Path.Combine(targetDirectoryWithBasePath, checksumDirectory)

    let sourceFileName = getFileName file
    let targetFileName = getChecksumFileName checksum sourceFileName
    let targetPath = Path.Combine(targetWithBasePath, targetFileName)
    checkIfFileExist targetPath

// The logic is filename with sha1-checksum meaning existed
let isFileExisted (fullPathInfo: PathInfo) (inputFile: File)  = 
    let (Name fileName) = inputFile.Name
    match fileName with 
    | RegexGroup FileWithChecksumRegex 0 fileName  -> 
        printfn $"File already existed with name: %s{fileName}" 
        true
    | _ -> 
        isInputFileExisted fullPathInfo inputFile

let getFilesPath (fullPathInfo: PathInfo) (files: File list) =
    files
    |> List.map (getChecksumFilePathInfo fullPathInfo)

let getTargetOutputWithChecksumFullPath (fullPathInfo: PathInfo) (checksum: string) =
    let targetDirectoryWithOutputFullPath = getFullPath fullPathInfo
    let checksumDirectory = getChecksumDirFromChecksum checksum
    let targetOutputFullPath = Path.Combine(targetDirectoryWithOutputFullPath, checksumDirectory)
    targetOutputFullPath

// End of Get

// Start of side effect (Copy or Create or Update)
let rec directoryCopy srcPath dstPath prefixForFileName copySubDirs  =

    if not <| Directory.Exists(srcPath) then
        let msg = String.Format("Source directory does not exist or could not be found: {0}", srcPath)
        raise (DirectoryNotFoundException(msg))

    if not <| Directory.Exists(dstPath) then
        Directory.CreateDirectory(dstPath) |> ignore

    let srcDir = DirectoryInfo(srcPath)

    for file in srcDir.GetFiles() do
        let fileNameWithPrefix = $"%s{prefixForFileName}-%s{file.Name}"
        let tempPath = Path.Combine(dstPath, fileNameWithPrefix)
        file.CopyTo(tempPath, true) |> ignore

    if copySubDirs then
        for subDir in srcDir.GetDirectories() do
            let dstSubDir = Path.Combine(dstPath, subDir.Name)
            directoryCopy subDir.FullName dstSubDir prefixForFileName copySubDirs

let createDirectoryIfNotExist path = 
    if (not <| checkIfDirectoryExist path) then
        let info = Directory.CreateDirectory path
        printfn $"Directory Created with info: %A{info}"
    else  
        printfn $"Directory(%A{path}) exists, no need to create."

let copyFile (sourceBasePath: string, sourceDirectory: string) (targetFullPath: PathInfo) (file: File) =
    let sourceFileNameWithFormat = getFileName file
    let sourceFileDir = Path.Combine (sourceBasePath, sourceDirectory)
    let sourcePath = Path.Combine (sourceFileDir, sourceFileNameWithFormat)
    let targetPath, targetDir, _ = getChecksumFilePathInfo targetFullPath file
    // Create directory before copying file
    createDirectoryIfNotExist targetDir
    if checkIfDirectoryExist targetDir then
        if checkIfFileExist targetPath then
            printfn $"File with path (%s{targetPath}) already exists, no need to copy file again."
        else
            File.Copy(sourcePath, targetPath)
            printfn $"%s{sourcePath} --- copied to ---> %s{targetPath}"
    else 
        printfn $"Target Dir (%s{targetDir}) does not exist, could not copy file to there"

    // compressFile targetPath (targetPath+".gz")
    // decompressFile (targetPath+".gz") (targetPath+".unzip")
    // sprintf "%s-%s" checksum fileName

let copyInputFiles (basePath: string, sourceDirectory: string, outputDirectory: string) (inputFiles: File list)  = 
    let fullPathInfo = { BasePath = basePath ; RelativePath = outputDirectory }
    inputFiles
    |> List.iter (copyFile (basePath, sourceDirectory) fullPathInfo)

let createFile (content: string, fileName: string, fileType: string, checksum: string) (fullPathInfo: PathInfo) = 
    // Get the file path info
    let checksumFileInfo = { FileName = fileName; Checksum = checksum }
    let { FileFullPath = targetPath ; FileDirFullPath = targetDir } = getFilePathInfo fullPathInfo checksumFileInfo 

    // Create the directory for the file
    createDirectoryIfNotExist targetDir

    if not <| checkIfFileExist targetPath then
        try
            let fs = IO.File.Create targetPath
            let info = Text.UTF8Encoding(true).GetBytes(content)
            fs.Write(info, 0, info.Length)
            printfn $"New %s{fileType} file Created at %s{targetPath}." 
            fs.Close()
        with 
        ex -> 
            printfn $"%s{fileType} file Content Not Created with Error: %s{ex.Message}"
    else 
        printfn $"TargetPath (%s{targetPath}) already exists"

let filterOutExistedFiles (targetPathInfo: PathInfo) (files: File list ) = 
    files
    |> List.filter ((isFileExisted targetPathInfo) >> not)

// Create the cal directory
let createCalDirectoryIfNotExist (basePath: string) =
    let calDirectoryFullPath = getCalDirectoryFullPath basePath 
    createDirectoryIfNotExist calDirectoryFullPath

// Create the simulation directory
let createSimulationDirectoryIfNotExist (checksum: string) (caseTitle: string) (basePath: string) (timestamp: string) = 
    let calDirectoryFullPath = getCalDirectoryFullPath basePath 
    // Create Sim dir
    let simDirectory = getSimulationDirectoryPath (checksum, caseTitle, timestamp)
    let fullPathInfo = { BasePath = calDirectoryFullPath; RelativePath = simDirectory }
    let simDirectoryWithBasePath = getFullPath fullPathInfo
    createDirectoryIfNotExist simDirectoryWithBasePath

    // Create Output dir inside Sim Dir
    let outputDirWithBasePath = Path.Combine(simDirectoryWithBasePath, "output")
    createDirectoryIfNotExist outputDirWithBasePath

let createSimulationFolder (checksum: string) (caseTitle: string) (basePath: string) (inputFiles: File list, inputTargetDir: string) (outputFiles: File list, outputTargetDir: string) =
    let currentTimeStamp = DateTime.Now.ToString("yyyyMMddHHmm")

    // Create Cal Dir
    createSimulationDirectoryIfNotExist checksum caseTitle basePath currentTimeStamp
    let fullPathInfo = { BasePath = basePath ; RelativePath = inputTargetDir }
    let inputFilesWithPath = getFilesPath fullPathInfo inputFiles
    let simDir = getSimulationDirectoryPath (checksum, caseTitle, currentTimeStamp) 

    // Create symbolic link for the input files
    inputFilesWithPath
    |> List.iter (
        fun file -> 
            let fileFullPath, _, targetFileName = file
            let calDirWithBasePath = Path.Combine(basePath, getCalDirectory)
            let simDirWithBasePath = Path.Combine(calDirWithBasePath, simDir)
            let symbolicLinkPath = Path.Combine(simDirWithBasePath, targetFileName)
            if checkIfFileExist symbolicLinkPath then
                printfn $"%s{symbolicLinkPath}--- symbolic link --->%s{fileFullPath} is already created, ignored"
            else
                System.IO.File.CreateSymbolicLink(symbolicLinkPath, fileFullPath) |> ignore
                printfn $"%s{symbolicLinkPath}--- symbolic link --->%s{fileFullPath} is Created"
    )
    
    // Create symbolic link for the output files
    let fullPathInfo = { BasePath = basePath ; RelativePath = outputTargetDir }
    let outputFilesWithPath = getFilesPath fullPathInfo outputFiles
    outputFilesWithPath
    |> List.iter (
        fun file -> 
            let fileFullPath, _, targetFileName = file
            let calDirWithBasePath = Path.Combine(basePath, getCalDirectory)
            let simDirWithBasePath = Path.Combine(calDirWithBasePath, simDir)
            let outputDirWithBasePath = Path.Combine(simDirWithBasePath, getTargetOutputDirectory)
            let symbolicLinkPath = Path.Combine(outputDirWithBasePath, targetFileName)
            if checkIfFileExist symbolicLinkPath then
                printfn $"%s{symbolicLinkPath}--- symbolic link --->%s{fileFullPath} is already created, ignored"
            else
                System.IO.File.CreateSymbolicLink(symbolicLinkPath, fileFullPath) |> ignore
                printfn $"%s{symbolicLinkPath}--- symbolic link --->%s{fileFullPath} is Created"
    )

let updateFile (content: string) (targetPath: string) = 
    if checkIfFileExist targetPath then
        try
            let fs = IO.File.AppendText targetPath
            let info = $"%s{Environment.NewLine}%s{content}"
            fs.WriteLine(info)
            printfn $"Content Updated with %A{info}" 
            fs.Close()
        with 
            ex -> 
                printfn $"Content did not updated, with Error: %s{ex.Message}"
    else 
        printfn $"TargetPath (%s{targetPath}) does not exists, did not update the content."
    