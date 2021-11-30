module Neo4j
open System
open Neo4jClient
open Neo4j.Driver
open FSharp.Json

open Dto
open Db
open Domain

open System.Collections.Generic;

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

let demoHasInput1 = HasInputDTO { SID = "1" }
let demoHasInput2 = HasInputDTO { SID = "2" }
let demoFileLocationis1 = FileLocationIsDTO { BasicPath = "root" }
let demoFileLocationis2 = FileLocationIsDTO { BasicPath = "src" }

let demoGrid = Grid {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c8a"
    NodeNumber = NodeNumber 4
}

let demoGridFile = File {
    Path = Path "/minio/demo/PO8/grid/"
    Name = Name "grid1"
    Format = Format "grd"
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c8z"
}

let demoFVCOMInput = FVCOMInput {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c10b"
    StartDate = FVCOMInput.StartDate "28/12/2021"
    EndDate = FVCOMInput.EndDate "29/12/2021"
    CaseTitle = FVCOMInput.CaseTitle "Titania"
    DateFormat = FVCOMInput.DateFormat"YMD"
    TimeZone = FVCOMInput.TimeZone "UTC"
}

let demoFVCOMInput1 = FVCOMInput {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c9b"
    StartDate = FVCOMInput.StartDate "28/12/2021"
    EndDate = FVCOMInput.EndDate "29/12/2021"
    CaseTitle = FVCOMInput.CaseTitle "Titania"
    DateFormat = FVCOMInput.DateFormat"YMD"
    TimeZone = FVCOMInput.TimeZone "UTC"
}

let demoFVCOMInput2 = FVCOMInput {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c8b"
    StartDate = FVCOMInput.StartDate "28/12/2021"
    EndDate = FVCOMInput.EndDate "29/12/2021"
    CaseTitle = FVCOMInput.CaseTitle "Titania"
    DateFormat = FVCOMInput.DateFormat"YMD"
    TimeZone = FVCOMInput.TimeZone "UTC"
}

let demoFVCOMInput3 = FVCOMInput {
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c11b"
    StartDate = FVCOMInput.StartDate "18/12/2021"
    EndDate = FVCOMInput.EndDate "19/12/2021"
    CaseTitle = FVCOMInput.CaseTitle "Titania"
    DateFormat = FVCOMInput.DateFormat"YMD"
    TimeZone = FVCOMInput.TimeZone "UTC"
}

let demoFVCOMInputFile = File {
    Path = Path "/minio/demo/PO8/"
    Name = Name "run"
    Format = Format "nml"
    Checksum = Checksum "163fbce9f5dfc1ea8355340bf35f68e20f3c7c8y"
}

// let initNodes = [demoGrid; demoFVCOMInputFile; demoGridFile; demoFVCOMInput]
// let initGrids = [demoGrid]
// let initGridsLabels = ["Grid"]
// let initFiles = [demoFVCOMInputFile; demoGridFile]
// let initFilesLabels = ["File"]
let initFVCOMInput = [demoFVCOMInput; demoFVCOMInput1; demoFVCOMInput2; demoFVCOMInput3; demoGrid; demoGridFile]
// let initFVCOMInputLabels = ["FVCOMInput"]

let getNodeLabel (node: Node) =
    let label =
        match node with
        | File _ -> nameof File
        | Grid _ -> nameof Grid
        | AirPressureInput _ -> nameof AirPressureInput
        | FVCOMInput _ -> nameof FVCOMInput
        | GridCoordinatesInput _ -> nameof GridCoordinatesInput
        | HeatingInput _ -> nameof HeatingInput
        | IOInput _ -> nameof IOInput
        | NetCDFInput _ -> nameof NetCDFInput
        | OBCInput _ -> nameof OBCInput
        | RiverInput _ -> nameof RiverInput
        | StartupInput _ -> nameof StartupInput
        | StartupXInput _ -> nameof StartupXInput
        | WaveInput _ -> nameof WaveInput
        | WindInput _ -> nameof WindInput
    label

// 10/11/2021
let getDataInDomainFormat<'T> (node: string) =
    Json.deserialize<'T> (node)

// 9/11/2021
let toListInDomainFormat (nodes: seq<string * seq<string>>) = 
    nodes 
    |> Seq.map (fun (dto) -> 
        dto |> NodeDto.toDomain
    ) 
    |> List.ofSeq

// 18/11/2021 & 19/11/2021
let getResult (direction: string) (pathResultSeq: seq<Neo4jClient.ApiModels.Cypher.PathsResultBolt>) = 
    pathResultSeq
    |> Seq.map (fun (pathResult) ->
        let relationships = pathResult.Relationships
        let nodes = pathResult.Nodes
        let nodedIdToNodeMap = 
            Seq.fold (fun (acc: Map<int64, Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode>) (node: Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode) -> acc.Add (node.Id, node)) (Map.empty) nodes
        Seq.fold (fun acc (relationship: Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltRelationship) ->
            let startNodeId = relationship.StartNodeId
            let endNodeId = relationship.EndNodeId
            let relationshipType = relationship.Type
            let startNode = nodedIdToNodeMap.Item startNodeId
            let endNode = nodedIdToNodeMap.Item endNodeId
            match direction with
            | "TO" -> 
                if acc = "" then
                    sprintf "NodeId:%i(%s) <-----%s----- NodeId:%i(%s)" endNodeId (Seq.reduce (+) endNode.Labels) relationshipType startNodeId (Seq.reduce (+) startNode.Labels)
                else 
                    sprintf "%s <-----%s----- NodeId:%i(%s)" acc relationshipType startNodeId (Seq.reduce (+) startNode.Labels)
            | "FROM" -> 
                if acc = "" then
                    sprintf "NodeId:%i(%s) -----%s-----> NodeId:%i(%s)" startNodeId (Seq.reduce (+) startNode.Labels) relationshipType endNodeId (Seq.reduce (+) endNode.Labels)
                else 
                    sprintf "%s -----%s-----> NodeId:%i(%s)" acc relationshipType endNodeId (Seq.reduce (+) endNode.Labels)
            | _ -> ""
        ) "" relationships 
    ) 
    |> List.ofSeq

// 22/11/2021
let getRelationshipAttributes (relationship: RelationshipDto) = 
    match relationship with
        | HasInputDTO v -> sprintf "{ SID: '%s' }" v.SID
        | FileLocationIsDTO v -> sprintf "{ BasicPath: '%s' }" v.BasicPath

let getRelationships (relationship: string, relationshipProperty: string option, relationshipPropertyValue: string option) =
    // let queryMatchTo = sprintf "p = (node)<-[relationship:%s {%s:'%s'}]-(targetNode)" relationship relationshipProperty relationshipPropertyValue
    let queryMatchTo = 
        match relationshipProperty with 
        | Some p -> 
            match relationshipPropertyValue with 
            | Some pv -> sprintf "p = (node)<-[relationship:%s {%s:'%s'}]-(targetNode)" relationship p pv
            | None -> sprintf "p = (node)<-[relationship:%s]-(targetNode)" relationship
        | None -> sprintf "p = (node)<-[relationship:%s]-(targetNode)" relationship
    let result =
        clientWithCypher.Match(queryMatchTo)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask |> Async.RunSynchronously 
    // for i in result do
    //     printfn "result: %s" i
    let resultTo =
        clientWithCypher.Match(queryMatchTo)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask |> Async.RunSynchronously 
        |> getResult "TO"
    resultTo |> List.distinct

let getRelatedNodesPath (relationship: string option, checksum: string, maxPathLength: string) =
    let queryMatchTo = 
        match relationship with
        | Some r -> sprintf "p = (node)<-[:%s%s]-(targetNode)" r maxPathLength
        | None -> sprintf "p = (node)<-[%s]-(targetNode)" maxPathLength
    let resultTo =
        clientWithCypher
            .Match(queryMatchTo)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask |> Async.RunSynchronously 
        |> getResult "TO"
    let queryMatchFrom = 
        match relationship with
        | Some r -> sprintf "p = (node)-[:%s%s]->(targetNode)" r maxPathLength
        | None -> sprintf "p = (node)-[%s]->(targetNode)" maxPathLength
    let resultFrom =
        clientWithCypher
            .Match(queryMatchFrom)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask |> Async.RunSynchronously 
        |> getResult "FROM"
    resultTo @ resultFrom

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
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
        |> toListInDomainFormat
    nodes

let getNodesByLabel (label: string) =
    let queryMatch = sprintf "(node: %s)" label
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
        |> toListInDomainFormat
    nodes
// 8/11/2021
let getClientWithNodeInputParameter (client: Cypher.ICypherFluentQuery) (node: Node) = 
    match node with 
        | File file -> client.WithParam("node", (FileDto.fromDomain file))
        | Grid grid -> client.WithParam("node", (GridDto.fromDomain grid))
        | AirPressureInput input -> client.WithParam("node", (AirPressureInputDto.fromDomain input))
        | FVCOMInput input -> client.WithParam("node", (FVCOMInputDto.fromDomain input))
        | GridCoordinatesInput input -> client.WithParam("node", (GridCoordinatesInputDto.fromDomain input))
        | HeatingInput input -> client.WithParam("node", (HeatingInputDto.fromDomain input))
        | IOInput input -> client.WithParam("node", (IOInputDto.fromDomain input))
        | NetCDFInput input -> client.WithParam("node", (NetCDFInputDto.fromDomain input))
        | OBCInput input -> client.WithParam("node", (OBCInputDto.fromDomain input))
        | RiverInput input -> client.WithParam("node", (RiverInputDto.fromDomain input))
        | StartupInput input -> client.WithParam("node", (StartupInputDto.fromDomain input))
        | StartupXInput input -> client.WithParam("node", (StartupXInputDto.fromDomain input))
        | WaveInput input -> client.WithParam("node", (WaveInputDto.fromDomain input))
        | WindInput input -> client.WithParam("node", (WindInputDto.fromDomain input))


let getNodeAttributes (node: Node) = 
    match node with
        | File file -> { Label = "File"; Key = "Checksum"; KeyValue = file.Checksum |> Checksum.value }
        | Grid grid -> { Label = "Grid"; Key = "Checksum"; KeyValue = grid.Checksum |> Checksum.value }
        | AirPressureInput input -> { Label = "AirPressureInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | FVCOMInput input -> { Label = "FVCOMInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | GridCoordinatesInput input -> { Label = "GridCoordinatesInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | HeatingInput input -> { Label = "HeatingInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | IOInput input -> { Label = "IOInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | NetCDFInput input -> { Label = "NetCDFInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | OBCInput input -> { Label = "OBCInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | RiverInput input -> { Label = "RiverInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | StartupInput input -> { Label = "StartupInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | StartupXInput input -> { Label = "StartupXInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | WaveInput input -> { Label = "WaveInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }
        | WindInput input -> { Label = "WindInput"; Key = "Checksum"; KeyValue = input.Checksum |> Checksum.value }

let getAllNodes () =
    let result =
        clientWithCypher
            .Match("(node)")
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask |> Async.RunSynchronously
        |> toListInDomainFormat
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

let relateNodes (sourceNode': Node) (targetNode': Node) (relationship: string) (relationshipProperty: RelationshipDto option) =
    let sourceNodeAttributes = getNodeAttributes(sourceNode')
    let targetNodeAttributes = getNodeAttributes(targetNode')
    let querySource = sprintf "(sourceNode:%s)" sourceNodeAttributes.Label 
    let queryTarget = sprintf "(targetNode:%s)" targetNodeAttributes.Label
    let queryRelationship = 
        match relationshipProperty with
        | Some p -> 
            let relationshipAttributes = getRelationshipAttributes(p)
            sprintf "(sourceNode)-[:%s%s]->(targetNode)" relationship relationshipAttributes
        | None -> sprintf "(sourceNode)-[:%s]->(targetNode)" relationship
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

let createInitNodesIfNotExist () = createMultipleNodesIfNotExist initFVCOMInput

let relateInitNodes () = 
    relateNodes demoFVCOMInput1 demoFVCOMInput2 "HAS_INPUT" (Some demoHasInput1)
    relateNodes demoFVCOMInput2 demoGrid "HAS_INPUT" (Some demoHasInput2)
    relateNodes demoFVCOMInput2 demoGridFile "FILE_LOCATION_IS" (Some demoFileLocationis2)
    relateNodes demoFVCOMInput2 demoFVCOMInputFile "FILE_LOCATION_IS" (Some demoFileLocationis1)
    relateNodes demoFVCOMInput demoFVCOMInput1 "HAS_INPUT" (Some demoHasInput1)
    relateNodes demoFVCOMInput3 demoFVCOMInput1 "HAS_INPUT" (Some demoHasInput1)
    // relateNodes demoFVCOMInput demoGrid "HAS_INPUT"
    printfn "%A" "relateInitNodes"

let createInitNodeIfNotExist () = createNodeIfNotExist demoGrid