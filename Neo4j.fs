module Neo4j
open Neo4jClient
open FSharp.Json

open Dto
open Domain

[<CLIMutable>]
type NodeAttributes = {
    Label: string
    Key: string
    // TODO find real type of Key value
    KeyValue: string
}
type Neo4jNode = {
    // Path: string
    // Name: string
    // Format: string
    Checksum: string
}

type RelationShipInfo = {
    SourceNode: Node 
    TargetNode: Node 
    Relationship: string 
    RelationshipProps: RelationshipDto option 
}
type Neo4jOutputData<'T> = {
    data: 'T
}

type Neo4jOutput<'T> = Neo4jOutputData<'T> * string * seq<string>

let clientWithCypher = Db.getDbClient ()

let getNodeLabel (node: Node) =
    let label =
        match node with
        | ConfigFileInput _ -> nameof ConfigFileInput
        | File _ -> nameof File
        | IOInput _ -> nameof IOInput
        | FVCOMInput _ -> nameof FVCOMInput
        | Simulation _ -> nameof Simulation
        // | Grid _ -> nameof Grid

        // | AirPressureInput _ -> nameof AirPressureInput
        
        // | GridCoordinatesInput _ -> nameof GridCoordinatesInput
        // | HeatingInput _ -> nameof HeatingInput
        // | NetCDFInput _ -> nameof NetCDFInput
        // | OBCElevationInput _ -> nameof OBCElevationInput
        // | OBCNodeListInput _ -> nameof OBCNodeListInput
        // | PrecipitationInput _ -> nameof PrecipitationInput
        // | RiverInput _ -> nameof RiverInput
        // | StartupInput _ -> nameof StartupInput
        // | StartupXInput _ -> nameof StartupXInput
        // | WaveInput _ -> nameof WaveInput
        // | WindInput _ -> nameof WindInput
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
        | HasInputDTO v -> sprintf "{ Type: '%s' }" v.Type
        | FileLocationIsDTO v -> sprintf "{ BasicPath: '%s' }" v.BasicPath
        | HasOutputDTO -> sprintf "{ }"
        | HasTreeDTO -> sprintf "{ }"
        | HasInputConfigDTO -> sprintf "{ }"

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

// 13/1/2022
let getClientWithCreateRelationStr (sourceNodeName, targetNodeName, relationship, relationshipProps) (client: Cypher.ICypherFluentQuery)   =
    let queryRelationshipStr = 
        match relationshipProps with
        | Some p -> 
            let relationshipAttributes = getRelationshipAttributes(p)
            sprintf "(%s)-[:%s%s]->(%s)" sourceNodeName relationship relationshipAttributes targetNodeName
        | None -> sprintf "(%s)-[:%s]->(%s)" sourceNodeName relationship targetNodeName
    client.Create(queryRelationshipStr)

let getClientWithMergeStr (str: string) (client: Cypher.ICypherFluentQuery) =
    client.Merge(str)

let getClientWithKeyValueParameter (key: string, value: string) (client: Cypher.ICypherFluentQuery)  =
    client.WithParam(key, value)

let getClientWithSetStr (str: string) (client: Cypher.ICypherFluentQuery)   =
    client.Set(str)

// 8/11/2021
let getClientWithNodeInputParameter (node: Node) (paramName: string) (client: Cypher.ICypherFluentQuery) = 
    client.WithParam(paramName, (fromDomain node))

let getNodeAttributes (node: Node) = 
    match node with
        | ConfigFileInput file -> { Label = "Config"; Key = "ConfigType"; KeyValue = file.ConfigType |> FileType.value }
        | Simulation simulation -> { Label = "Simulation"; Key = "Checksum"; KeyValue = simulation.Checksum |> Checksum.value }
        | File file -> { Label = "File"; Key = "Checksum"; KeyValue = file.Checksum |> Checksum.value }
        | FVCOMInput input -> { Label = "FVCOMInput"; Key = "ConfigType"; KeyValue = input.ConfigType |> InputType.value }
        | IOInput input -> { Label = "IOInput"; Key = "ConfigType"; KeyValue = input.ConfigType |> InputType.value }

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

let convertNodeToQueryStr (index: int) (node: Node) =
    let nodeAttributes = getNodeAttributes node
    let nodeLabel = nodeAttributes.Label
    let nodeKey = nodeAttributes.Key
    let nodeKeyValue = nodeAttributes.KeyValue
    let mergeStr = sprintf "(node%i:%s {%s: $nodeKeyValue%i})" index nodeLabel nodeKey index
    (mergeStr, index, node, nodeKeyValue)

let getReducedMergeQueryStr (strList: string list) = 
    List.reduce (
        fun acc str -> 
            sprintf "%s,%s" acc str
    ) strList

let createNodesIfNotExist (nodes: Node list) =
    let queriesForNodes = List.mapi convertNodeToQueryStr nodes
    let clientWithSetStrAndParam = 
        List.fold (
            fun acc (mergeStr, index, node, nodeKeyValue) -> 
                let nodeName = sprintf "node%i" index
                let setStr = sprintf "%s = $%s" nodeName nodeName
                let nodeKey = sprintf "nodeKeyValue%i" index 
                let client' = acc |> getClientWithMergeStr mergeStr
                client'.OnCreate()
                |> getClientWithSetStr setStr
                |> getClientWithNodeInputParameter node nodeName
                |> getClientWithKeyValueParameter (nodeKey, nodeKeyValue)
                
        ) clientWithCypher queriesForNodes
        
    clientWithSetStrAndParam.ExecuteWithoutResultsAsync()
    |> Async.AwaitTask 
    |> Async.RunSynchronously

let createSingleNodeIfNotExist (node: Node) =
    let nodes = [node]
    createNodesIfNotExist nodes

let createMultipleNodesIfNotExist (nodes: Node list) =
    try
        createNodesIfNotExist nodes
    with
    | error ->
        let message = sprintf "Exception in creating nodes: %s" error.Message
        printfn "%A" message

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
    // printfn "relationshipProperty: %A" relationshipProperty
    // printfn "queryRelationship: %A" queryRelationship
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
            let client' = getClientWithNodeInputParameter node "node" clientWithCypher
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
            let client' = getClientWithNodeInputParameter node "node" clientWithCypher
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

let convertRelationToQueryStr (index: int) (relationShipInfo: RelationShipInfo) =
    let sourceNode = relationShipInfo.SourceNode
    let targetNode = relationShipInfo.TargetNode
    let relationship = relationShipInfo.Relationship
    let relationshipProps = relationShipInfo.RelationshipProps
    let sourceNodeAttributes = getNodeAttributes(sourceNode)
    let targetNodeAttributes = getNodeAttributes(targetNode)
    let sourceNodeName = sprintf "%s%i" "sourceNode" index
    let targetNodeName = sprintf "%s%i" "targetNode" index
    let querySourceStr = sprintf "(%s:%s{ %s: '%s' })" sourceNodeName sourceNodeAttributes.Label sourceNodeAttributes.Key sourceNodeAttributes.KeyValue
    let queryTargetStr = sprintf "(%s:%s{ %s: '%s' })" targetNodeName targetNodeAttributes.Label sourceNodeAttributes.Key targetNodeAttributes.KeyValue
    let queryRelationshipStr = 
        match relationshipProps with
        | Some p -> 
            let relationshipAttributes = getRelationshipAttributes(p)
            sprintf "(%s)-[:%s%s]->(%s)" sourceNodeName relationship relationshipAttributes targetNodeName
        | None -> sprintf "(%s)-[:%s]->(%s)" sourceNodeName relationship targetNodeName
    (querySourceStr, queryTargetStr, queryRelationshipStr, index, sourceNodeName, sourceNodeAttributes.KeyValue, targetNodeName, targetNodeAttributes.KeyValue, relationship)

let createNodesRelationship (relationShipInfos: RelationShipInfo list)=
    let queriesForNodes = List.mapi convertRelationToQueryStr relationShipInfos
    let clientWithMatchParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (querySourceStr, queryTargetStr, queryRelationshipStr, index, sourceNodeName, sourceNodeKeyValue, targetNodeName, targetNodeKeyValue, relationship) -> 
                acc.Match(querySourceStr, queryTargetStr)
        ) clientWithCypher queriesForNodes
    let clientWithCreateParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (querySourceStr, queryTargetStr, queryRelationshipStr, index, sourceNodeName, sourceNodeKeyValue, targetNodeName, targetNodeKeyValue, relationship) -> 
                acc.Create(queryRelationshipStr)
        ) clientWithMatchParam queriesForNodes
    clientWithCreateParam.ExecuteWithoutResultsAsync()
    |> Async.AwaitTask 
    |> Async.RunSynchronously

let relateMultipleNodes (relationShipInfos: RelationShipInfo list) =
    try
        createNodesRelationship relationShipInfos
    with
    | error ->
        let message = sprintf "Exception in creating nodes relationship: %s" error.Message
        printfn "%A" message