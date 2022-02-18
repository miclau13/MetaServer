module Db

open Command
open Dto
open Logger
open Neo4jDbHelper

module Neo4jDbService =
    // Get
    let getAllNodes () =
        let getAllNodesApiResult = getAllNodesApi ()
        match getAllNodesApiResult with
        | Ok nodeDTOReturnData -> 
            let nodeResults = nodeDTOReturnData |> NodeDto.nodeDTOsToDomain
            List.iter (
                function
                | Ok node ->
                    printfn $"getAllNodesApiResult node: {node}"
                | Error error ->
                    printfn $"getAllNodesApiResult DTO error: {error}"
            ) nodeResults
            nodeResults
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error
    let getAllRelationships input =
        let result = getAllRelationshipsApi input
        match result with
        | Ok relationshipsDTOData ->
             relationshipsDTOData
             |> List.ofSeq
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error
    let getNodesByChecksum checksum =
        let getNodesByChecksumApiResult = getNodesByChecksumApi checksum
        match getNodesByChecksumApiResult with
        | Ok nodeDTOReturnData -> 
            let nodeResults = nodeDTOReturnData |> NodeDto.nodeDTOsToDomain
            List.iter (
                function
                | Ok node ->
                    printfn $"getNodesByChecksum node: {node}"
                | Error error ->
                    printfn $"getNodesByChecksum DTO error: {error}"
            ) nodeResults
            nodeResults
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error    
    
    let getNodesByLabel label =
        let result = getNodesByLabelApi label
        match result with
        | Ok nodeDTOReturnData -> 
            let nodeResults = nodeDTOReturnData |> NodeDto.nodeDTOsToDomain
            printfn $"getNodesByLabel nodeResults: {nodeResults}"
            nodeResults
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error    
    let getPaths input =
        let result = getPathsApi input
        match result with
        | Ok pathDTOData ->
            let direction = input.Direction
            let nodeResults = getPathResult direction pathDTOData
            nodeResults
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error
    let getPathsByNodeChecksum input =
        let result = getPathsByNodeChecksumApi input
        match result with
        | Ok pathDTOData ->
            let direction = input.Direction
            let nodeResults = getPathResult direction pathDTOData
            nodeResults
        | Error error ->
             globalLogger.Error error
             // TODO
             failwith error
    // Create
    let createNodes nodes =
        let createNodesApiResult = createNodesApi nodes
        match createNodesApiResult with
        | Ok _ ->
            let logMsg = $"Nodes ({nodes}) are created successfully."
            globalLogger.Info logMsg
        | Error error ->
            globalLogger.Error error
    let createNode node =
        createNodes [node]
    
    let createNodesRelationship input =
        let result = createNodesRelationshipApi input
        match result with
        | Ok _ ->
            let logMsg = $"Nodes Relationship ({input}) are created successfully."
            globalLogger.Info logMsg
        | Error error ->
            globalLogger.Error error
    // Delete
    let deleteAllNodes () =
        let deleteAllNodesApiResult = deleteAllNodesApi ()
        match deleteAllNodesApiResult with
        | Ok _ ->
            let logMsg = $"All nodes are deleted successfully."
            globalLogger.Info logMsg
        | Error error ->
            globalLogger.Error error
