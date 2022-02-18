module Neo4jDbHelper

open Domain
open Dto
open Neo4jClient
open Neo4jDb

// Relationship 
let getInputRelationshipInfos (simulationNode: Node) (fileNodes: FileNode list) =
    let inputRelationshipInfos: RelationShipInfo list = 
        List.map (
            fun (item: FileNode) -> 
                let { Node = inputFile ; Type = relationship } = item
                let relationshipProps = Some (HasInputDTO { Type = relationship })
                { SourceNode = simulationNode ; TargetNode = inputFile ; Relationship = "HAS_INPUT" ; RelationshipProps = relationshipProps }
        ) fileNodes
    inputRelationshipInfos

let getInputConfigFileRelationshipInfo  (simulationNode: Node) (inputConfigFileNode: Node) = 
    let inputConfigFileRelationshipInfo: RelationShipInfo = 
        { SourceNode = simulationNode ; TargetNode = inputConfigFileNode ; Relationship = "HAS_INPUT_CONFIG" ; RelationshipProps = None }
    inputConfigFileRelationshipInfo

let getPathResult (direction: PathDirection) (pathResultSeq: PathDTOData) = 
    pathResultSeq
    |> Seq.map (fun pathResult ->
        let relationships = pathResult.Relationships
        let nodes = pathResult.Nodes
        let nodeIdToNodeMap = 
            Seq.fold (fun (acc: Map<int64, ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode>) (node: ApiModels.Cypher.PathsResultBolt.PathsResultBoltNode) -> acc.Add (node.Id, node)) Map.empty nodes
        Seq.fold (fun acc (relationship: ApiModels.Cypher.PathsResultBolt.PathsResultBoltRelationship) ->
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

let getTreeFileRelationshipInfo (simulationNode: Node) (treeFileNode: Node) =
    let treeFileRelationshipInfo: RelationShipInfo = 
        { SourceNode = simulationNode ; TargetNode = treeFileNode ; Relationship = "HAS_TREE" ; RelationshipProps = None }
    treeFileRelationshipInfo

let getOutputRelationshipInfos (simulationNode: Node) (outputFileNodes: Node list) =
    let outputRelationshipInfos: RelationShipInfo list = 
        List.map (
            fun file -> 
                { SourceNode = simulationNode ; TargetNode = file ; Relationship = "HAS_OUTPUT" ; RelationshipProps = None }
        ) outputFileNodes
    outputRelationshipInfos