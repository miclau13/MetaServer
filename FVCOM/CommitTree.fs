module FVCOM.CommitTree

open Domain
open FileIO
open Neo4jDb
open System
open Util

type TreeFileInfo = {
    Name: string
    Type: string
}

let getTreeFileInfo () =
    { Name = "tree.txt"; Type = "Tree" }

let getTreeFileTargetPathInfo (checksum: string) (pathInfo: PathInfo) =
    let { Name = treeFileName; Type = _ } = getTreeFileInfo ()
    let treeChecksumFileInfo = { FileName = treeFileName; Checksum = checksum }
    getFilePathInfo pathInfo treeChecksumFileInfo 

let getTreeFileNode (checksum: string) (targetFullPath: PathInfo) =
    let { Name = treeFileName; Type = treeFileType } = getTreeFileInfo ()
    let { FileDirFullPath = treeFileTargetDir } = getTreeFileTargetPathInfo checksum targetFullPath
    let treeFileNameWithChecksum = getChecksumFileName checksum treeFileName
    let treeFileNode = 
        Input.getInputFileResult treeFileNameWithChecksum treeFileTargetDir treeFileType
        |> Option.get
        |> (fun item -> item.Node)
    treeFileNode
    
let getTreeFileRelationshipInfo (simulationNode: Node) (treeFileNode: Node) =
    let treeFileRelationshipInfo: RelationShipInfo = 
        { SourceNode = simulationNode ; TargetNode = treeFileNode ; Relationship = "HAS_TREE" ; RelationshipProps = None }
    treeFileRelationshipInfo
    
let getTreeRelatedFilesStr (files: File List) =
    let commitChecksums = files |> getChecksumListArrayFromFiles
    let checksumStr = getChecksumStrFromChecksumArray commitChecksums
    checksumStr

let createTreeFile (checksum: string) (files: File list) = 
    let checksumStr = getTreeRelatedFilesStr files
    let currentTimeStamp = DateTime.UtcNow.ToString()
    let currentUser = Environment.UserName
    let commit = getChecksumFileName checksum "tree"
    let treeContent = 
        $"commit %s{commit}\nAuthor: %s{currentUser}\nDate: %s{currentTimeStamp}\nRelated files: \n%s{checksumStr}"
    let { Name = treeFileName; Type = treeFileType } = getTreeFileInfo ()
 
    createFile (treeContent, treeFileName, treeFileType, checksum) 

// Append the files name to related files
let updateTreeRelatedFiles (checksum: string) (pathInfo: PathInfo) (files: File list)   = 
    let { FileFullPath = targetPath } = getTreeFileTargetPathInfo checksum pathInfo
    let checksumStr = getTreeRelatedFilesStr files
    updateFile checksumStr targetPath
