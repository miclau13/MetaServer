module FileIO

open Command
open Domain
open System

type FileContent = string

let createDirectory (destDirFullPath: FullPath) =
    createTestDirectoryApi destDirFullPath |> ignore

let createFile (fileName: RelativePath, fileChecksum: Checksum, fileContent: FileContent, destDirFullPath: FullPath) =
    let fileChecksumDirFullPath = getFileChecksumDirFullPath (fileChecksum, destDirFullPath)
    createTestDirectoryApi fileChecksumDirFullPath |> ignore
    
    let destInputFileName = getChecksumFileName fileChecksum fileName
    let destInputFilePath = getFullPath(fileChecksumDirFullPath, destInputFileName)
    createTestFileApi (destInputFilePath, fileContent) |> ignore

module InputFile = 
    type CreateInputFilesInput = {
        InputFiles: File list
        SourceDirPath: FullPath
        TargetDirPath: FullPath
    }
    let createInputFile (sourceDirPath: FullPath, targetDirPath: FullPath) (file: File) =
        let fileName = getFileName file
        let fileChecksum = file.Checksum
        let fileChecksumDir = getChecksumDirFromChecksum fileChecksum
        let fileChecksumDirFullPath = getFullPath(targetDirPath, fileChecksumDir)
        createTestDirectoryApi fileChecksumDirFullPath |> ignore
        
        let sourceInputFilePath = getFullPath(sourceDirPath, fileName)
        let destInputFileName = getChecksumFileName fileChecksum fileName
        let destInputFilePath = getFullPath(fileChecksumDirFullPath, destInputFileName)
        copyTestFileApi (sourceInputFilePath, destInputFilePath) |> ignore
    let createInputFiles createFilesInput =
        let {
            InputFiles = inputFiles
            SourceDirPath = sourceDirPath
            TargetDirPath = targetDirPath
        } = createFilesInput
        inputFiles
        |> List.toArray
        |> Array.Parallel.iter (
            createInputFile(sourceDirPath, targetDirPath)
        )

module CalculationDirectory =
    type CaseTitle = string

    type CreateSimulationDirInput = {
        BasePath: BasePath
        CaseTitle: CaseTitle
        Checksum: Checksum
        FilesTargetPath: FullPath
        InputFiles: File list
        OutputFiles: File list
    }
    let getCalDirectory () = RelativePath "cal"
    let getCalOutputDirectory () = RelativePath "output"
    let getCalDirFullPath (basePath: BasePath) =
        let calDirectory = getCalDirectory()
        getFullPath(basePath, calDirectory)

    let getSimulationDirectoryName (checksum: Checksum, caseTitle: CaseTitle) =
        let (Checksum checksum') = checksum
        let currentTimeStamp = DateTime.Now.ToString("yyyyMMddHHmm")
        RelativePath $"%s{checksum'}-%s{caseTitle}-%s{currentTimeStamp}"
        
    let createSymbolicLink (symbolicLinkDirPath: FullPath, filesTargetPath: FullPath) (file: File) =
        let fileChecksum = file.Checksum
        let symbolicLinkName = getFileNameWithChecksum file
        let pathTargetDir = getFileChecksumDirFullPath(fileChecksum, filesTargetPath)
        let pathTarget = getFullPath(pathTargetDir, symbolicLinkName)
        let symbolicLinkPath = getFullPath(symbolicLinkDirPath, symbolicLinkName)
        createTestSymbolicLinkApi (symbolicLinkPath, pathTarget)
        |> ignore
    let createSimulationDir createSimulationDirInput =
        let {
            BasePath = basePath
            CaseTitle = caseTitle
            Checksum = checksum
            FilesTargetPath = filesTargetPath
            InputFiles = inputFiles
            OutputFiles = outputFiles
        } = createSimulationDirInput
        // Create Cal Dir
        let calDirectoryFullPath = getCalDirFullPath(basePath)
        createTestDirectoryApi calDirectoryFullPath |> ignore
        // Create Cal checksum dir inside Cal dir
        let calChecksumDir = getSimulationDirectoryName(checksum, caseTitle)
        let calChecksumDirectoryFullPath = getFullPath(calDirectoryFullPath, calChecksumDir)
        createTestDirectoryApi calChecksumDirectoryFullPath |> ignore
        // Create output dir inside Cal checksum dir
        let calChecksumOutputDirectoryFullPath = getFullPath(calChecksumDirectoryFullPath, getCalOutputDirectory())
        createTestDirectoryApi calChecksumOutputDirectoryFullPath |> ignore
        // Create symbolic link for the input files
        inputFiles
        |> List.toArray
        |> Array.Parallel.iter (
            createSymbolicLink(calChecksumDirectoryFullPath, filesTargetPath)
        )
        // Create symbolic link for the output files
        outputFiles
        |> List.toArray
        |> Array.Parallel.iter (
            createSymbolicLink(calChecksumOutputDirectoryFullPath, filesTargetPath)
        )
        
module CommitFile =
    open FVCOM.CommitTree
    type CreateCommitFileInput = {
        Checksum: Checksum
        FileTargetPath: FullPath
        InputFiles: File list
    }
    
    type UpdateCommitFileInput = {
        Checksum: Checksum
        FileTargetPath: FullPath
        OutputFiles: File list
    }
    
    let createCommitFile (createCommitFileInput: CreateCommitFileInput) =
        let {
            Checksum = checksum
            FileTargetPath = fileTargetPath
            InputFiles = inputFiles
        } = createCommitFileInput
        let checksumStr = getTreeRelatedFilesStr inputFiles
        let currentTimeStamp = DateTime.UtcNow.ToString()
        let currentUser = Environment.UserName
        let commitFileName = getTreeFileName ()
        let (RelativePath commit) = getChecksumFileName checksum commitFileName
        let treeContent = 
            $"commit %s{commit}\nAuthor: %s{currentUser}\nDate: %s{currentTimeStamp}\nRelated files: \n%s{checksumStr}"
        createFile(commitFileName, checksum, treeContent, fileTargetPath)
    
    let updateCommitFile (updateCommitFileInput: UpdateCommitFileInput) =
        let {
            Checksum = checksum
            FileTargetPath = fileTargetPath
            OutputFiles = outputFiles
        } = updateCommitFileInput
        let outputChecksumStr = getTreeRelatedFilesStr outputFiles
        let treeChecksumDirFullPath = getFileChecksumDirFullPath (checksum, fileTargetPath)
        let commitFileName = getTreeFileName ()
        let treeDestFileName = getChecksumFileName checksum commitFileName
        let treeDestFileFullPath = getFullPath(treeChecksumDirFullPath, treeDestFileName)
        updateTestFileApi (treeDestFileFullPath, outputChecksumStr) |> ignore
module OutputFile =
    type CopyOutputDirInput = {
        Checksum: Checksum
        FilesSourcePath: FullPath
        FilesTargetPath: FullPath
    }
    
    let copyOutputDir (copyOutputDirInput: CopyOutputDirInput) =
        let {
            Checksum = checksum
            FilesSourcePath = sourceDir
            FilesTargetPath = destDir
        } = copyOutputDirInput
                       
        let (Checksum outputChecksum) = checksum
        copyTestDirectoryApi (sourceDir, destDir, Some(outputChecksum)) |> ignore