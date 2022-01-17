module FileIO

open System
open System.IO
open Domain

let getAllFilesInDirectory srcPath =
    if not <| Directory.Exists(srcPath) then
        let msg = System.String.Format("Source directory does not exist or could not be found: {0}", srcPath)
        raise (DirectoryNotFoundException(msg))

    let srcDir = new DirectoryInfo(srcPath)
    srcDir.GetFiles()
    

let rec directoryCopy srcPath dstPath prefixForFileName copySubDirs  =

    if not <| Directory.Exists(srcPath) then
        let msg = System.String.Format("Source directory does not exist or could not be found: {0}", srcPath)
        raise (DirectoryNotFoundException(msg))

    if not <| Directory.Exists(dstPath) then
        Directory.CreateDirectory(dstPath) |> ignore

    let srcDir = new DirectoryInfo(srcPath)

    for file in srcDir.GetFiles() do
        // let (fName, _) = Input.getFileNameAndFormat (file.Name)
        let fileNameWithPrefix = sprintf "%s-%s" prefixForFileName file.Name
        let temppath = Path.Combine(dstPath, fileNameWithPrefix)
        file.CopyTo(temppath, true) |> ignore

    if copySubDirs then
        for subdir in srcDir.GetDirectories() do
            let dstSubDir = Path.Combine(dstPath, subdir.Name)
            directoryCopy subdir.FullName dstSubDir prefixForFileName copySubDirs

let getChecksumInfoFromChecksumArray (checksums: string []) = 
    let checksumStr = 
        checksums 
        |> Array.reduce (fun acc item -> 
            sprintf "%s\n%s" acc item
        ) 
    let checksum = 
        checksumStr
        |> Input.getChecksum
    (checksum, checksumStr)

let getFileNameWithFormat (file: File) =
    let (Name fileName) = file.Name
    let (Format fileFormat) = file.Format
    sprintf "%s.%s" fileName fileFormat

let getChecksumFileName (checksum: string) (fileName: string) = 
    match fileName with 
    | Util.RegexGroup "(\w{40}-)(.*)" 0 _  -> 
        fileName
    | _ ->  sprintf "%s-%s" checksum fileName

let getFullPathWithBasePath (basePath: string) (path: string) =
    match basePath with 
    | "." | "" | "./" -> 
        let currentDirectory = Directory.GetCurrentDirectory()
        Path.Combine(currentDirectory, path)
    | p -> 
        Path.Combine(p, path)

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

// Modified on 12 Jan - Use one directory level only
let getPathInfoFromChecksum (checksum: string) = 
    let directoryLevel1 = checksum.[0..1]
    // let directoryLevel2 = checksum.[2..3]
    let fileName = checksum.[0..]
    // let path = sprintf "/%s/" directoryLevel1 
    let path = directoryLevel1
    (path, directoryLevel1, fileName)

let getChecksumFilePathInfo (basePath: string, directory: string) (file: File) =
    let directoryWithBasePath = getFullPathWithBasePath basePath directory
    let (Checksum checksum) = file.Checksum
    let (checksumDirectory, _, checksumFileName) = getPathInfoFromChecksum checksum
    let filePathWithChecksumDir = Path.Combine(directoryWithBasePath, checksumDirectory)
    let fileNameWithFormat = getFileNameWithFormat file
    let targetFileName = getChecksumFileName checksumFileName fileNameWithFormat
    let fullPath = Path.Combine(filePathWithChecksumDir, targetFileName)
    fullPath, filePathWithChecksumDir, targetFileName

let copyFile (sourceBasePath: string, sourceDirectory: string) (targetBasePath: string, targetDirectory: string) (file: File) =
    let sourceFileNameWithFormat = getFileNameWithFormat file
    let sourceFileDir = Path.Combine (sourceBasePath, sourceDirectory)
    let sourcePath = Path.Combine (sourceFileDir, sourceFileNameWithFormat)
    let (targetPath, targetDir, _) = getChecksumFilePathInfo (targetBasePath, targetDirectory) (file)
    // Create directory before copying file
    createDirectoryIfNotExist targetDir
    if checkIfDirectoryExist targetDir then
        File.Copy(sourcePath, targetPath)
        printfn "%s --- copied to ---> %s" sourcePath targetPath
    else 
        printfn "Target Dir (%s) does not exist, could not copy file to there" targetDir

    // compressFile targetPath (targetPath+".gz")
    // decompressFile (targetPath+".gz") (targetPath+".ungz")
    // sprintf "%s-%s" checksum fileName

let copyInputFiles (basePath: string, sourceDirectory: string, outputDirectory: string) (inputFiles: list<Domain.Node>)  = 
    inputFiles
    |> Array.ofList
    |> Array.Parallel.iter (fun item -> 
        match item with 
        | Domain.File f -> copyFile (basePath, sourceDirectory) (basePath, outputDirectory) f
        | _ ->  printfn "Item is not copied because it is not defined in the domain: %A" item
    )
    // |> ignore

let createTreeFile (basePath, targetDirectory) (commitChecksums: string []) = 
    let currentTimeStamp = DateTime.UtcNow.ToString()
    let currentUser = Environment.UserName

    let (checksum, checksumStr) = getChecksumInfoFromChecksumArray commitChecksums
    // printfn "createTreeFile commitChecksums: %A" commitChecksums
    // printfn "createTreeFile checksumStr: %s" checksumStr
    
    // Create the directory for the tree
    let targetDirectoryWithBasePath = getFullPathWithBasePath basePath targetDirectory
    createDirectoryIfNotExist targetDirectoryWithBasePath
    
    let (checksumDirectory, _, checksumFileName) = getPathInfoFromChecksum checksum
    let targetFileName = getChecksumFileName checksumFileName "tree"
    let targetWithBasePath = Path.Combine(targetDirectoryWithBasePath, checksumDirectory)
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

// The logic is filename with sha1-checksum meaning existed
let isOutputFileExisted (fileName: string) =
    match fileName with 
    | Util.RegexGroup "(\w{40}-)(.*)" 0 fileName  -> 
        printfn "Output file already existed with name: %s" fileName 
        true
    | _ -> false

let isInputFileExisted (targetBasePath: string, targetDirectory: string) (file: File) =
    let targetDirectoryWithBasePath = getFullPathWithBasePath targetBasePath targetDirectory

    let (Checksum checksum) = file.Checksum
    let (checksumDirectory, _, checksumFileName) = getPathInfoFromChecksum checksum
    let targetWithBasePath = Path.Combine(targetDirectoryWithBasePath, checksumDirectory)

    let sourceFileName = getFileNameWithFormat file
    let targetFileName = getChecksumFileName checksumFileName sourceFileName
    let targetPath = Path.Combine(targetWithBasePath, targetFileName)
    checkIfFileExist targetPath

// Do not copy existing file again 
let isFileExisted (targetBasePath: string, targetDirectory: string) (inputFile: File)  = 
    let (Name fileName) = inputFile.Name
    (isOutputFileExisted fileName) || (isInputFileExisted (targetBasePath, targetDirectory) inputFile)

let filterOutExistedFiles (targetBasePath: string, targetDirectory: string) (inputFiles: list<Domain.Node> ) = 
    inputFiles
    |> List.filter(fun item -> 
        match item with 
        | File f -> not <| isFileExisted (targetBasePath, targetDirectory)  f
        | _ ->  
            printfn "Item is not defined in the domain" 
            true
    )

let getCalDirectory = "cal"

let getCalDirectoryFullPath (basePath: string) = 
    let calDirectory = getCalDirectory
    let calDirectoryWithBasePath = getFullPathWithBasePath basePath calDirectory
    calDirectoryWithBasePath

let getSimulationDirectoryPath (checksum: string, caseTitle: string, timestamp: string) = 
    sprintf "%s-%s-%s" checksum caseTitle timestamp

// Create the cal directory
let createCalDirectoryIfNotExist (basePath: string) =
    let calDirectoryFullPath = getCalDirectoryFullPath basePath 
    if not <| checkIfDirectoryExist calDirectoryFullPath then
        createDirectoryIfNotExist calDirectoryFullPath

// Create the simulation directory
let createSimulationDirectoryIfNotExist (checksum: string) (caseTitle: string) (basePath: string) (timestamp: string) = 
    // createCalDirectoryIfNotExist basePath
    let calDirectoryFullPath = getCalDirectoryFullPath basePath 

    let simDirectory = getSimulationDirectoryPath (checksum, caseTitle, timestamp)
    let simDirectoryWithBasePath = getFullPathWithBasePath calDirectoryFullPath simDirectory
    if not <| checkIfDirectoryExist simDirectoryWithBasePath then
        createDirectoryIfNotExist simDirectoryWithBasePath

let getFilePathResult (basePath: string, outputDirectory: string) (node: Node) =
    match node with 
    | Domain.File file -> 
        let info = getChecksumFilePathInfo (basePath, outputDirectory) (file)
        Some info
    | _ -> 
        printfn "The Node is not a file, cannot get file Path: %A" node
        None

let getFilesPath (basePath: string, outputDirectory: string) (nodes: Node list) =
    nodes
    |> List.map (fun file -> 
            getFilePathResult (basePath, outputDirectory) file
        )
    |> List.filter (Option.isSome)
    |> List.map (Option.get)

let getOutputTargetPath (basePath: string, outputDirectory: string) (inputListChecksum: string) = 
    let (checksumDirectory, _, _) = getPathInfoFromChecksum inputListChecksum
    let targetDirectoryWithBasePath = getFullPathWithBasePath basePath outputDirectory
    let targetWithBasePath = Path.Combine(targetDirectoryWithBasePath, "output")
    targetWithBasePath
    // let dstPath = Path.Combine(targetWithBasePath, checksumDirectory)
    // dstPath

let getOutputTargetPathWithChecksumDir (path: string,  checksum: string) = 
    let (checksumDirectory, _, _) = getPathInfoFromChecksum checksum
    let dstPath = Path.Combine(path, checksumDirectory)
    dstPath

let createSimulationFolder (checksum: string) (caseTitle: string) (basePath: string) (inputFiles: Node list, inputTargetDir: string) (outputFiles: Node list, outputTargetDir: string) =
    let currentTimeStamp = DateTime.Now.ToString("yyyy-MM-ddTHH:mm:ss.sssZ")

    // Create Cal Dir
    createSimulationDirectoryIfNotExist (checksum) (caseTitle) (basePath) (currentTimeStamp)

    let inputFilesWithPath = getFilesPath (basePath, inputTargetDir) inputFiles
    let simDir = getSimulationDirectoryPath (checksum, caseTitle, currentTimeStamp) 

    // Create symbolic link for the input files
    inputFilesWithPath
    |> Array.ofList
    |> Array.Parallel.iter (
        fun file -> 
            let (fileFullPath, filePathWithChecksumDir, targetFileName) = file
            let calDirWithBasePath = Path.Combine(basePath, getCalDirectory)
            let simDirWithBasePath = Path.Combine(calDirWithBasePath, simDir)
            let symbolicLinkPath = Path.Combine(simDirWithBasePath, targetFileName)
            System.IO.File.CreateSymbolicLink(symbolicLinkPath, fileFullPath) |> ignore
    )
    
     // Create symbolic link for the output files
    let outputFilesWithPath = getFilesPath (basePath, outputTargetDir) outputFiles
    outputFilesWithPath
    |> Array.ofList
    |> Array.Parallel.iter (
        fun file -> 
            let (fileFullPath, filePathWithChecksumDir, targetFileName) = file
            let calDirWithBasePath = Path.Combine(basePath, getCalDirectory)
            let simDirWithBasePath = Path.Combine(calDirWithBasePath, simDir)
            let symbolicLinkPath = Path.Combine(simDirWithBasePath, targetFileName)
            // if not <| checkIfFileExist filePathWithChecksumDir then
                // createDirectoryIfNotExist simDirectoryWithBasePath
            System.IO.File.CreateSymbolicLink(symbolicLinkPath, fileFullPath) |> ignore
    )


