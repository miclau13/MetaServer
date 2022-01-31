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
    
let createTreeFile (checksumStr: string) (checksum: string) = 
    
    let currentTimeStamp = DateTime.UtcNow.ToString()
    let currentUser = Environment.UserName
    let commit = getChecksumFileName checksum "tree"
    let treeContent = 
        $"commit %s{commit}\nAuthor: %s{currentUser}\nDate: %s{currentTimeStamp}\nRelated files: \n%s{checksumStr}"
    let { Name = treeFileName; Type = treeFileType } = getTreeFileInfo ()
    createFile (treeContent, treeFileName, treeFileType, checksum)
    
let getTreeFileNode (checksum: string) (targetFullPath: FullPathInfo) =
    let { Name = treeFileName; Type = treeFileType } = getTreeFileInfo ()
    let treeChecksumFileInfo = { FileName = treeFileName; Checksum = checksum }
    let { FileDirFullPath = treeFileTargetDir } = getFilePathInfo targetFullPath treeChecksumFileInfo
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