module Neo4j
open System
open Neo4jClient
open Neo4j.Driver
open FSharp.Json

open Dto
open Db
open Domain

[<CLIMutable>]
type NodeAttributes = {
    Label: string
    Key: string
    // TODO find real type of Key value
    KeyValue: string
}
type File = {
    Path: string
    Name: string
    Format: string
    Checksum: string
}

type Neo4jOutputData<'T> = {
    data: 'T
}

type Neo4jOutput<'T> = Neo4jOutputData<'T> * string * seq<string>

type FVCOMInput = {
    Checksum: string
    StartDate: string
    EndDate: string
}
type River = {
    Checksum: string
    RiverName: string
    RiverGridLocation: int
    RiverVerticalDistribution: list<float>
    RiverFile: string
}

let clientWithCypher = Db.getDbClient ()

let demoGrid = Grid {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c6a"
    NodeNumber = NodeNumber 3
}

let demoGridFile = File {
    Path = Path "/minio/demo/PO7/grid/"
    Name = Name "grid1"
    Format = Format "grd"
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c6z"
}

let demoFVCOMInput = FVCOMInput {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c6b"
    StartDate = StartDate "08/11/2021"
    EndDate = EndDate "09/11/2021"
}

let demoFVCOMInputFile = File {
    Path = Path "/minio/demo/PO7/"
    Name = Name "run"
    Format = Format "nml"
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c6y"
}

// let runNML = {
//     Path = "/work/jonas/Ti1"
//     Name = "Ti1_run"
//     Format = "nml"
//     Checksum = "883caa0d769cf15d52a25916105db81152fd5a4e78642a7f39d57e13421bf7e4"
// }

// let riverNamelistNML = {
//     Path = "/work/jonas/Ti1"
//     Name = "RiverNamelist"
//     Format = "nml"
//     Checksum = "1942d76f855b979531bae445451e1ffbd8b168ea0f01f1d1511c38e23c678d66"
// }

// let river1 = {
//     RiverName = "026.Z - SIRA _ 02"
//     Checksum = "xab"
//     RiverGridLocation = 435
//     RiverVerticalDistribution = [0.00177;0.00210]
//     RiverFile = "riverdata.nc"
// }

let initNodes = [demoGrid; demoFVCOMInputFile; demoGridFile; demoFVCOMInput]
let initGrids = [demoGrid]
let initGridsLabels = ["Grid"]
let initFiles = [demoFVCOMInputFile; demoGridFile]
let initFilesLabels = ["File"]
let initFVCOMInput = [demoFVCOMInput]
let initFVCOMInputLabels = ["FVCOMInput"]

let getNodeLabel (node: Node) =
    let label =
        match node with
        | File _ -> nameof File
        | Grid _ -> nameof Grid
        | FVCOMInput _ -> nameof FVCOMInput
    label


// 10/11/2021
let getDataInDomainFormat<'T> (node: string) =
    Json.deserialize<'T> (node)

// 9/11/2021
let toListInDomainFormat (nodes: seq<string * string * seq<string>>) = 
    nodes 
        |> Seq.map (fun (dto) -> 
            dto |> Node.toDomain
        ) 
        |> List.ofSeq

let getAllRelationship () =
    let queryMatch = sprintf "(node)-[r]->(result)" 
    let result =
        clientWithCypher
            .Match(queryMatch)
            .ReturnDistinct(fun (r: Cypher.ICypherResultItem) -> r.Type())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
    nodes

let getNodeByChecksum (checksum: string) =
    let queryMatch = sprintf "(node)"
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
    nodes

let getRelatedNodes (relationship: string option, checksum: string) =
    let queryMatch = 
        match relationship with
        | Some r -> sprintf "(node)-[relationship:%s]->(targetNode)" r
        | None -> sprintf "(node)<-[relationship]->(targetNode)"
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun targetNode relationship -> targetNode.As(), relationship.Type(), targetNode.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously 
    let data = toListInDomainFormat nodes
    data

let getNodesByLabel (label: string) =
    let queryMatch = sprintf "(node: %s)" label
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
    nodes

// 8/11/2021
let getClientWithNodeInputParameter (client: Cypher.ICypherFluentQuery) (node: Node) = 
    match node with 
        | File file -> client.WithParam("node", file)
        | Grid grid -> client.WithParam("node", grid)
        | FVCOMInput fvcomInput -> client.WithParam("node", fvcomInput)

let getNodeAttributes (node: Node) = 
    match node with
        | File file -> { Label = "File"; Key = "Checksum"; KeyValue = file.Checksum |> Checksum.value }
        | Grid grid -> { Label = "Grid"; Key = "Checksum"; KeyValue = grid.Checksum |> Checksum.value }
        | FVCOMInput fvcomInput -> { Label = "FVCOMInput"; Key = "Checksum"; KeyValue = fvcomInput.Checksum |> Checksum.value }

let getAllNodes () =
    let result =
        clientWithCypher
            .Match("(node)")
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
    nodes

let createNodeIfNotExist (node: Node) =
    let nodeAttributes = getNodeAttributes node
    let nodeLabel = nodeAttributes.Label
    let nodeKey = nodeAttributes.Key
    let nodeKeyValue = nodeAttributes.KeyValue
    let query = sprintf "(node:%s {%s: $nodeKeyValue})"  nodeLabel nodeKey
    let client' = getClientWithNodeInputParameter clientWithCypher node

    client'
        .Merge(query)
        .OnCreate()
        .Set("node = $node")
        .WithParam("nodeKeyValue", nodeKeyValue)
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let createMultipleNodesIfNotExist nodes =
    try
        List.iter createNodeIfNotExist nodes
        let message = sprintf "%A are created successfully!" nodes
        Ok(nodes, message)
    with
    | error ->
        let message = sprintf "Exception in creating nodes: %s" error.Message
        Error(message)

let relateNodes (sourceNode': Node) (targetNode': Node) (relationship: string) =
    let sourceNodeAttributes = getNodeAttributes(sourceNode')
    let targetNodeAttributes = getNodeAttributes(targetNode')
    let querySource = sprintf "(sourceNode:%s)" sourceNodeAttributes.Label 
    let queryTarget = sprintf "(targetNode:%s)" targetNodeAttributes.Label
    let queryRelationship = sprintf "(sourceNode)-[:%s]->(targetNode)" relationship
    clientWithCypher
        .Match(querySource, queryTarget)
        .Where(fun (sourceNode) -> sourceNode.Checksum = sourceNodeAttributes.KeyValue)
        .AndWhere(fun (targetNode) -> targetNode.Checksum = targetNodeAttributes.KeyValue)
        .Create(queryRelationship)
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let deleteNode (node: Node) =
    let nodeAttributes = getNodeAttributes(node)
    let query = sprintf "(node:%s)" nodeAttributes.Label
    clientWithCypher
        .Match(query)
        .Where(fun (node) -> node.Checksum = nodeAttributes.KeyValue)
        .DetachDelete("node")
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let deleteAllNodes () =
    clientWithCypher
        .Match("(node)")
        .DetachDelete("node")
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let create (node: Node) =

    let cypherCreate (node: Node) =
        try
            let label = getNodeLabel node
            let query = sprintf "(n:%s $node)" label 
            let client' = getClientWithNodeInputParameter clientWithCypher node
            client'
                .Create(query)
                .ExecuteWithoutResultsAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously

            let message = sprintf "%s is created successfully!" label
            Ok(node, message)
        with
        | error ->
            let message = sprintf "Exception in creating node: %s" error.Message
            Error(message)

    cypherCreate node

let update (node: Node) =

    let cypherUpdate (node: Node) =
        try
            let label = getNodeLabel node
            let query = sprintf "(n:%s)" label
            let client' = getClientWithNodeInputParameter clientWithCypher node
            client'
                .Match(query)
                .ExecuteWithoutResultsAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously

            let message = sprintf "%s is created successfully!" label
            Ok(node, message)
        with
        | error ->
            let message = sprintf "Exception in creating node: %s" error.Message
            Error(message)

    cypherUpdate node

let deleteDemoGrid () = deleteNode demoGrid

let createInitNodesIfNotExist () = createMultipleNodesIfNotExist initNodes

let relateInitNodes () = 
    relateNodes demoFVCOMInput demoGrid "HAS_INPUT"
    relateNodes demoGrid demoGridFile "FILE_LOCATION_IS"
    relateNodes demoFVCOMInput demoFVCOMInputFile "FILE_LOCATION_IS"

let createInitNodeIfNotExist () = createNodeIfNotExist demoGrid