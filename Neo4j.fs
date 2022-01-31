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
    label

// 10/11/2021
let getDataInDomainFormat<'T> (node: string) =
    Json.deserialize<'T> node

// 9/11/2021
let toListInDomainFormat (nodes: seq<string * seq<string>>) = 
    nodes 
    |> Seq.map (fun dto -> 
        dto |> NodeDto.toDomain
    ) 
    |> List.ofSeq

// 18/11/2021 & 19/11/2021
let getResult (direction: string) (pathResultSeq: seq<Neo4jClient.ApiModels.Cypher.PathsResultBolt>) = 
    pathResultSeq
    |> Seq.map (fun pathResult ->
        let relationships = pathResult.Relationships
        let nodes = pathResult.Nodes
        let nodeIdToNodeMap = 
            Seq.fold (fun (acc: Map<int64, Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode>) (node: Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode) -> acc.Add (node.Id, node)) Map.empty nodes
        Seq.fold (fun acc (relationship: Neo4jClient.ApiModels.Cypher.PathsResultBolt.PathsResultBoltRelationship) ->
            let startNodeId = relationship.StartNodeId
            let endNodeId = relationship.EndNodeId
            let relationshipType = relationship.Type
            let startNode = nodeIdToNodeMap.Item startNodeId
            let endNode = nodeIdToNodeMap.Item endNodeId
            match direction with
            | "TO" -> 
                if acc = "" then
                    $"NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels}) <-----%s{relationshipType}----- NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels})"
                else 
                    $"%s{acc} <-----%s{relationshipType}----- NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels})"
            | "FROM" -> 
                if acc = "" then
                    $"NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels}) -----%s{relationshipType}-----> NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels})"
                else 
                    $"%s{acc} -----%s{relationshipType}-----> NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels})"
            | _ -> ""
        ) "" relationships 
    ) 
    |> List.ofSeq

// 22/11/2021
let getRelationshipAttributes (relationship: RelationshipDto) = 
    match relationship with
        | HasInputDTO v -> $"{{ Type: '%s{v.Type}' }}"
        | FileLocationIsDTO v -> $"{{ BasicPath: '%s{v.BasicPath}' }}"
        | HasOutputDTO -> sprintf "{ }"
        | HasTreeDTO -> sprintf "{ }"
        | HasInputConfigDTO -> sprintf "{ }"

let getRelationships (relationship: string, relationshipProperty: string option, relationshipPropertyValue: string option) =
    let queryMatchTo = 
        match relationshipProperty with 
        | Some p -> 
            match relationshipPropertyValue with 
            | Some pv -> $"p = (node)<-[relationship:%s{relationship} {{%s{p}:'%s{pv}'}}]-(targetNode)"
            | None -> $"p = (node)<-[relationship:%s{relationship}]-(targetNode)"
        | None -> $"p = (node)<-[relationship:%s{relationship}]-(targetNode)"

    clientWithCypher.Match(queryMatchTo)
        .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
        .ResultsAsync
    |> Async.AwaitTask |> Async.RunSynchronously
    |> ignore
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
        | Some r -> $"p = (node)<-[:%s{r}%s{maxPathLength}]-(targetNode)"
        | None -> $"p = (node)<-[%s{maxPathLength}]-(targetNode)"
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
        | Some r -> $"p = (node)-[:%s{r}%s{maxPathLength}]->(targetNode)"
        | None -> $"p = (node)-[%s{maxPathLength}]->(targetNode)"
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
    let queryMatch = $"(node: %s{label})"
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
            $"(%s{sourceNodeName})-[:%s{relationship}%s{relationshipAttributes}]->(%s{targetNodeName})"
        | None -> $"(%s{sourceNodeName})-[:%s{relationship}]->(%s{targetNodeName})"
    client.Create(queryRelationshipStr)

let getClientWithMergeStr (str: string) (client: Cypher.ICypherFluentQuery) =
    client.Merge(str)

let getClientWithKeyValueParameter (key: string, value: string) (client: Cypher.ICypherFluentQuery)  =
    client.WithParam(key, value)

let getClientWithSetStr (str: string) (client: Cypher.ICypherFluentQuery)   =
    client.Set(str)

// 8/11/2021
let getClientWithNodeInputParameter (node: Node) (paramName: string) (client: Cypher.ICypherFluentQuery) = 
    let nodeDto = fromDomain node
    match nodeDto with 
    | FileDto dto -> client.WithParam(paramName, dto)
    | ConfigFileInputDto dto -> client.WithParam(paramName, dto)
    | IOInputDto dto -> client.WithParam(paramName, dto)
    | FVCOMInputDto dto -> client.WithParam(paramName, dto)
    | SimulationDto dto -> client.WithParam(paramName, dto)

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
    let mergeStr = $"(node%i{index}:%s{nodeLabel} {{%s{nodeKey}: $nodeKeyValue%i{index}}})"
    (mergeStr, index, node, nodeKeyValue)

let getReducedMergeQueryStr (strList: string list) = 
    List.reduce (
        fun acc str -> 
            $"%s{acc},%s{str}"
    ) strList

let createNodesIfNotExist (nodes: Node list) =
    let queriesForNodes = List.mapi convertNodeToQueryStr nodes
    let clientWithSetStrAndParam = 
        List.fold (
            fun acc (mergeStr, index, node, nodeKeyValue) -> 
                let nodeName = $"node%i{index}"
                let setStr = $"%s{nodeName} = $%s{nodeName}"
                let nodeKey = $"nodeKeyValue%i{index}" 
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
        let message = $"Exception in creating nodes: %s{error.Message}"
        printfn $"%A{message}"

let relateNodes (sourceNode': Node) (targetNode': Node) (relationship: string) (relationshipProperty: RelationshipDto option) =
    let sourceNodeAttributes = getNodeAttributes(sourceNode')
    let targetNodeAttributes = getNodeAttributes(targetNode')
    let querySource = $"(sourceNode:%s{sourceNodeAttributes.Label})" 
    let queryTarget = $"(targetNode:%s{targetNodeAttributes.Label})"
    let queryRelationship = 
        match relationshipProperty with
        | Some p -> 
            let relationshipAttributes = getRelationshipAttributes(p)
            $"(sourceNode)-[:%s{relationship}%s{relationshipAttributes}]->(targetNode)"
        | None -> $"(sourceNode)-[:%s{relationship}]->(targetNode)"
    clientWithCypher
        .Match(querySource, queryTarget)
        .Where(fun sourceNode -> sourceNode.Checksum = sourceNodeAttributes.KeyValue)
        .AndWhere(fun targetNode -> targetNode.Checksum = targetNodeAttributes.KeyValue)
        .Create(queryRelationship)
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let deleteNode (node: Node) =
    let nodeAttributes = getNodeAttributes(node)
    let query = $"(node:%s{nodeAttributes.Label})"
    clientWithCypher
        .Match(query)
        .Where(fun node -> node.Checksum = nodeAttributes.KeyValue)
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
            let query = $"(n:%s{label} $node)" 
            let client' = getClientWithNodeInputParameter node "node" clientWithCypher
            client'
                .Create(query)
                .ExecuteWithoutResultsAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously

            let message = $"%s{label} is created successfully!"
            Ok(node, message)
        with
        | error ->
            let message = $"Exception in creating node: %s{error.Message}"
            Error(message)

    cypherCreate node

let update (node: Node) =

    let cypherUpdate (node: Node) =
        try
            let label = getNodeLabel node
            let query = $"(n:%s{label})"
            let client' = getClientWithNodeInputParameter node "node" clientWithCypher
            client'
                .Match(query)
                .ExecuteWithoutResultsAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously

            let message = $"%s{label} is created successfully!"
            Ok(node, message)
        with
        | error ->
            let message = $"Exception in creating node: %s{error.Message}"
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
    let querySourceStr = $"(%s{sourceNodeName}:%s{sourceNodeAttributes.Label}{{ %s{sourceNodeAttributes.Key}: '%s{sourceNodeAttributes.KeyValue}' }})"
    let queryTargetStr = $"(%s{targetNodeName}:%s{targetNodeAttributes.Label}{{ %s{sourceNodeAttributes.Key}: '%s{targetNodeAttributes.KeyValue}' }})"
    let queryRelationshipStr = 
        match relationshipProps with
        | Some p -> 
            let relationshipAttributes = getRelationshipAttributes(p)
            $"(%s{sourceNodeName})-[:%s{relationship}%s{relationshipAttributes}]->(%s{targetNodeName})"
        | None -> $"(%s{sourceNodeName})-[:%s{relationship}]->(%s{targetNodeName})"
    (querySourceStr, queryTargetStr, queryRelationshipStr, index, sourceNodeName, sourceNodeAttributes.KeyValue, targetNodeName, targetNodeAttributes.KeyValue, relationship)

let createNodesRelationship (relationShipInfos: RelationShipInfo list)=
    let queriesForNodes = List.mapi convertRelationToQueryStr relationShipInfos
    let clientWithMatchParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (querySourceStr, queryTargetStr, _, _, _, _, _, _, _) -> 
                acc.Match(querySourceStr, queryTargetStr)
        ) clientWithCypher queriesForNodes
    let clientWithCreateParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (_, _, queryRelationshipStr, _, _, _, _, _, _) -> 
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
        let message = $"Exception in creating nodes relationship: %s{error.Message}"
        printfn $"%A{message}"