module FVCOM.CommitTree

open Domain
open Util

let getTreeFileName () = RelativePath "tree.txt"
let getTreeFileType () = "Tree"
    
let getTreeRelatedFilesStr (files: File List) =
    let commitChecksums = files |> getChecksumListArrayFromFiles
    let checksumStr = getChecksumStrFromChecksumArray commitChecksums
    checksumStr

let getTreeFileNode (checksum: Checksum) (targetFullPath: FullPath) =
    let fileName = getTreeFileName ()
    let (RelativePath fileNameWithChecksum) = Domain.getChecksumFileName checksum fileName
    let configType = FileType (getTreeFileType ())
    let file = InputFile fileNameWithChecksum
    let configNode: ConfigFileInput = {
          ConfigType = configType
          File = file
    }
    let treeFileDir = Domain.getChecksumDirFromChecksum checksum
    let treeFileDirFullPath = Domain.getFullPath(targetFullPath, treeFileDir)
    let treeFileConfigFileNode = 
        tryConvertFileNodeFromConfigFile treeFileDirFullPath configNode
        |> Option.get
        |> (fun item -> item.Node)
    treeFileConfigFileNode
