module Db

open Command
open Dto
open Logger
open Neo4jDbHelper

module Neo4jDbService =
    let printUnitResult (successStr: string) (result: Result<unit, exn>) =
        match result with
        | Ok _ ->
            let logMsg = $"{successStr}"
            logInfoMsg logMsg
        | Error exn ->
             logErrorMsg $"{exn}"
    let printNodesResult (result: Result<'a, exn>) =
        match result with
        | Ok dataResult ->
            match dataResult with
            | Ok nodes ->
                logInfoMsg $"{nodes}"
            | Error dtoError ->
                 let error = $"{dtoError}"
                 logErrorMsg error
        | Error exn ->
             logErrorMsg $"{exn}"
    
    let printResult (result: Result<string list, exn>) =
        match result with
        | Ok data ->
           logInfoMsg $"{data}"
        | Error exn ->
           logErrorMsg $"{exn}"
    // Get
    let getAllNodes () =
        let result = getAllNodesApi ()
        Result.map NodeDto.nodeDTOsToDomain result
    let getAllRelationships input =
        let result = getAllRelationshipsApi input
        Result.map List.ofSeq result
    let getNodesByChecksum checksum =
        let result = getNodesByChecksumApi checksum
        Result.map NodeDto.nodeDTOsToDomain result  
    
    let getNodesByLabel label =
        let result = getNodesByLabelApi label
        Result.map NodeDto.nodeDTOsToDomain result  
    let getPaths input =
        let result = getPathsApi input
        let direction = input.Direction
        Result.map (getPathResult direction) result  
    let getPathsByNodeChecksum input =
        let result = getPathsByNodeChecksumApi input
        let direction = input.Direction
        Result.map (getPathResult direction) result
        
    // Create
    let createNodes nodes =
        let result = createNodesApi nodes
        printUnitResult $"Nodes ({nodes}) are created successfully." result
    let createNode node =
        createNodes [node]
    let createNodesRelationship input =
        let result = createNodesRelationshipApi input
        printUnitResult $"Nodes Relationship ({input}) are created successfully." result
        
    // Delete
    let deleteAllNodes () =
        let result = deleteAllNodesApi ()
        printUnitResult $"All nodes are deleted successfully." result
