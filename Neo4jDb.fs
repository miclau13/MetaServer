module Neo4jDb

open DbConfig
open Domain
open Dto
open Neo4jClient

[<CLIMutable>]
type NodeAttributes = {
    Label: string
    Key: string
    // TODO find real type of Key value
    KeyValue: string
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

let clientWithCypher = getDbClient ()

// 18/11/2021 & 19/11/2021
let getPathResult (direction: PathDirection) (pathResultSeq: seq<Neo4jClient.ApiModels.Cypher.PathsResultBolt>) = 
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
            | TO -> 
                if acc = "" then
                    $"NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels}) <-----%s{relationshipType}----- NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels})"
                else 
                    $"%s{acc} <-----%s{relationshipType}----- NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels})"
            | FROM -> 
                if acc = "" then
                    $"NodeId:%i{startNodeId}(%s{Seq.reduce (+) startNode.Labels}) -----%s{relationshipType}-----> NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels})"
                else 
                    $"%s{acc} -----%s{relationshipType}-----> NodeId:%i{endNodeId}(%s{Seq.reduce (+) endNode.Labels})"
        ) "" relationships 
    ) 
    |> List.ofSeq

// 22/11/2021
let getRelationshipAttributes (relationship: RelationshipDto) = 
    match relationship with
        | HasInputDTO v ->
            let (FileType fileType) = v.Type
            $"{{ Type: '%s{fileType}' }}"
        | FileLocationIsDTO v -> $"{{ BasicPath: '%s{v.BasicPath}' }}"
        | HasOutputDTO -> sprintf "{ }"
        | HasTreeDTO -> sprintf "{ }"
        | HasInputConfigDTO -> sprintf "{ }"

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

let getNodeAttributes (node: Node) : NodeAttributes = 
    match node with
        | ConfigFileInput file -> { Label = "Config"; Key = "ConfigType"; KeyValue = file.ConfigType |> FileType.value }
        | Simulation simulation -> { Label = "Simulation"; Key = "Checksum"; KeyValue = simulation.Checksum |> Checksum.value }
        | File file -> { Label = "File"; Key = "Checksum"; KeyValue = file.Checksum |> Checksum.value }
        | FVCOMInput input -> { Label = "FVCOMInput"; Key = "ConfigType"; KeyValue = input.ConfigType |> InputType.value }
        | IOInput input -> { Label = "IOInput"; Key = "ConfigType"; KeyValue = input.ConfigType |> InputType.value }

let convertNodeToQueryStr (index: int) (node: Node) =
    let nodeAttributes = getNodeAttributes node
    let nodeLabel = nodeAttributes.Label
    let nodeKey = nodeAttributes.Key
    let nodeKeyValue = nodeAttributes.KeyValue
    let mergeStr = $"(node%i{index}:%s{nodeLabel} {{%s{nodeKey}: $nodeKeyValue%i{index}}})"
    (mergeStr, index, node, nodeKeyValue)

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
    (querySourceStr, queryTargetStr, queryRelationshipStr)

// Get
let getAllNodesAsync () : Async<NodeDTOReturnData> =
    let result =
        clientWithCypher
            .Match("(node)")
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodesAS =
        result |> Async.AwaitTask 
    nodesAS

let getAllRelationshipsAsync () =
    let queryMatch = sprintf "(node)-[r]->(result)" 
    let result =
        clientWithCypher
            .Match(queryMatch)
            .ReturnDistinct(fun (r: Cypher.ICypherResultItem) -> r.Type())
            .ResultsAsync
    let relationships =
        result |> Async.AwaitTask 
    relationships
let getNodesByChecksumAsync (checksum: Checksum) =
    let (Checksum checksum) = checksum
    let queryMatch = sprintf "(node)"
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask 
    nodes

let getNodesByLabelAsync (label: NodeLabel) =
    let queryMatch = $"(node: %s{label})"
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Return(fun (node: Cypher.ICypherResultItem) -> node.As(), node.Labels())
            .ResultsAsync
    let nodes =
        result |> Async.AwaitTask
    nodes

let getPathsByNodeChecksumAsync (relationship: string option, direction: PathDirection, checksum: string, maxPathLength: string) =
    let queryMatch = 
        match relationship with
        | Some r ->
            match direction with
            | TO -> $"p = (node)<-[:%s{r}%s{maxPathLength}]-(targetNode)"
            | FROM -> $"p = (node)-[:%s{r}%s{maxPathLength}]->(targetNode)"
        | None ->
            match direction with
            | TO -> $"p = (node)<-[%s{maxPathLength}]-(targetNode)"
            | FROM -> $"p = (node)-[%s{maxPathLength}]->(targetNode)"
    let result =
        clientWithCypher
            .Match(queryMatch)
            .Where(fun node -> node.Checksum = checksum)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask 
    result

let getPathsAsync (relationship: string, relationshipProperty: string option, relationshipPropertyValue: string option, direction: PathDirection) =
    let baseQuery =
        match direction with
        | TO -> $"p = (node)<-[relationship:%s{relationship}]-(targetNode)"
        | FROM ->  $"p = (node)-[relationship:%s{relationship}]->(targetNode)"
    
    let queryMatch = 
        match relationshipProperty with 
        | Some p -> 
            match relationshipPropertyValue with 
            | Some pv ->
                match direction with
                | TO -> $"p = (node)<-[relationship:%s{relationship} {{%s{p}:'%s{pv}'}}]-(targetNode)"
                | FROM -> $"p = (node)-[relationship:%s{relationship} {{%s{p}:'%s{pv}'}}]->(targetNode)"
            | None -> baseQuery
        | None -> baseQuery

    let result =
        clientWithCypher.Match(queryMatch)
            .Return(fun (p: Cypher.ICypherResultItem) -> p.As())
            .ResultsAsync
        |> Async.AwaitTask
    result

// Create
let createNodesIfNotExistAsync (nodes: Node list) =
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

let createNodesRelationshipAsync (relationShipInfos: RelationShipInfo list)=
    let queriesForNodes = List.mapi convertRelationToQueryStr relationShipInfos
    let clientWithMatchParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (querySourceStr, queryTargetStr, _) -> 
                acc.Match(querySourceStr, queryTargetStr)
        ) clientWithCypher queriesForNodes
    let clientWithCreateParam = 
        List.fold (
            fun (acc: Cypher.ICypherFluentQuery) (_, _, queryRelationshipStr) -> 
                acc.Create(queryRelationshipStr)
        ) clientWithMatchParam queriesForNodes
    clientWithCreateParam.ExecuteWithoutResultsAsync()
    |> Async.AwaitTask 

// Delete
let deleteAllNodesAsync () =
    clientWithCypher
        .Match("(node)")
        .DetachDelete("node")
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask