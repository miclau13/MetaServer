module DbInstruction

open Domain
open Dto
open Instruction
open InterpretationProgram
open Neo4jDb

type Decision =
  | CreateNodesDecision of Node list
  | CreateNodesRelationshipDecision of RelationShipInfo list
  | DeleteAllNodesDecision of unit
  | NoAction

type DbInstruction<'a> =
  | CreateNodes of Node list * next:(unit -> 'a)
  | CreateNodesRelationship of RelationShipInfo list * next:(unit -> 'a)
  | DeleteAllNodes of unit * next:(unit -> 'a)
  | GetAllNodes of unit * next:(NodeDTOReturnData -> 'a)
  | GetAllRelationships of unit * next:(RelationshipDTOData -> 'a)
  | GetNodesByChecksum of Checksum * next:(NodeDTOReturnData -> 'a)
  | GetNodesByLabel of NodeLabel * next:(NodeDTOReturnData -> 'a)
  | GetPaths of GetPathsInput * next:(PathDTOData -> 'a)
  | GetPathsByNodeChecksum of GetPathsByNodeInput * next:(PathDTOData -> 'a)

  interface IInstruction<'a> with
    member this.Map f =
      match this with
      | CreateNodes (nodes, next) ->
          CreateNodes (nodes, next >> f)
      | CreateNodesRelationship (input, next) ->
          CreateNodesRelationship (input, next >> f)
      | DeleteAllNodes (_, next) ->
          DeleteAllNodes ((), next >> f)
      | GetAllNodes (_, next) ->
          GetAllNodes ((), next >> f)
      | GetAllRelationships (_, next) ->
          GetAllRelationships ((), next >> f)
      | GetNodesByChecksum (checksum, next) ->
          GetNodesByChecksum (checksum, next >> f)
      | GetNodesByLabel (label, next) ->
          GetNodesByLabel (label, next >> f)
      | GetPaths (input, next) ->
          GetPaths (input, next >> f)
      | GetPathsByNodeChecksum (input, next) ->
          GetPathsByNodeChecksum (input, next >> f)
      :> IInstruction<_>

let createNodes nodes =
  Instruction (CreateNodes(nodes, Stop))
let createNodesRelationship input =
  Instruction (CreateNodesRelationship(input, Stop))
let deleteAllNodes _ =
  Instruction (DeleteAllNodes((), Stop))
let getAllNodes _ =
  Instruction (GetAllNodes((), Stop))
let getAllRelationships _ =
  Instruction (GetAllRelationships((), Stop))
let getNodesByChecksum checksum =
  Instruction (GetNodesByChecksum(checksum, Stop))
let getNodesByLabel label =
  Instruction (GetNodesByLabel(label, Stop))
let getPaths input =
  Instruction (GetPaths(input, Stop))
let getPathsByNodeChecksum input =
  Instruction (GetPathsByNodeChecksum(input, Stop))

module Pure =
  let deleteAllNodes _ =
    program {
        return DeleteAllNodesDecision ()
    }
  let createNodes (nodes: Node list) =
    program {
        return CreateNodesDecision nodes
    }
  let createNodesRelationship (input: RelationShipInfo list) =
   program {
        return CreateNodesRelationshipDecision input
   }

module Shell =
  let handleDecision (decision:Decision) :Program<unit> =
    match decision with
    | CreateNodesDecision nodes ->
        createNodes nodes
    | CreateNodesRelationshipDecision createNodesRelationshipInput ->
        createNodesRelationship createNodesRelationshipInput
    | DeleteAllNodesDecision deleteAllNodesInput ->
        deleteAllNodes deleteAllNodesInput
    | NoAction ->
        program.Zero()
  let createNodes nodes =
    program {
      let! decision = Pure.createNodes nodes
      do! handleDecision decision
    }
  let createNodesRelationship createNodesRelationshipInput =
    program {
      let! decision = Pure.createNodesRelationship createNodesRelationshipInput
      do! handleDecision decision
    }
  let deleteAllNodes deleteAllNodesInput =
    program {
      let! decision = Pure.deleteAllNodes deleteAllNodesInput
      do! handleDecision decision
    }
  let getAllNodes _ =
    program {
        return! getAllNodes ()
    }
  let getAllRelationships _ =
    program {
        return! getAllRelationships ()
    }
  let getNodesByChecksum checksum =
    program {
        return! getNodesByChecksum checksum
    }
  let getNodesByLabel label =
    program {
        return! getNodesByLabel label
    }
  let getPaths input =
    program {
        return! getPaths input
    }
  let getPathsByNodeChecksum input =
    program {
        return! getPathsByNodeChecksum input
    }
    
module Impure =
  let createNodes nodes =
    createNodesIfNotExistAsync nodes |> AsyncResult.asyncReturn
  let createNodesRelationship (input: RelationShipInfo list) =
    createNodesRelationshipAsync input |> AsyncResult.asyncReturn
  let deleteAllNodes _ =
    deleteAllNodesAsync() |> AsyncResult.asyncReturn
  let getAllNodes _ =
    getAllNodesAsync() |> AsyncResult.asyncReturn
  let getAllRelationships _ =
    getAllRelationshipsAsync() |> AsyncResult.asyncReturn
  let getNodesByChecksum checksum =
    getNodesByChecksumAsync checksum |> AsyncResult.asyncReturn
  let getNodesByLabel label =
    getNodesByLabelAsync label |> AsyncResult.asyncReturn
  let getPaths input =
      let direction = input.Direction
      let relationship = input.Relationship
      let relationshipPropertyOpt = input.RelationshipPropertyOpt
      let relationshipPropertyValueOpt = input.RelationshipPropertyValueOpt
      getPathsAsync (relationship, relationshipPropertyOpt, relationshipPropertyValueOpt, direction)
      |> AsyncResult.asyncReturn 
  let getPathsByNodeChecksum (input: GetPathsByNodeInput) =
      let direction = input.Direction
      let (Checksum checksum) = input.Checksum
      let maxPathLength = input.MaxPathLength
      let relationshipOpt = input.RelationshipOpt
      getPathsByNodeChecksumAsync (relationshipOpt, direction, checksum, maxPathLength)
      |> AsyncResult.asyncReturn
    
let interpretDbInstruction interpret (inst: DbInstruction<'a>) =
  match inst with
  | CreateNodes (nodes, next) ->
      let unitAS = Impure.createNodes nodes
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | CreateNodesRelationship (createNodesRelationshipInput, next) ->
      let unitAS = Impure.createNodesRelationship createNodesRelationshipInput
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | DeleteAllNodes (_, next) ->
      let unitAS = Impure.deleteAllNodes ()
      let newProgramAS = (AsyncResult.map next) unitAS
      interpret newProgramAS
  | GetAllNodes (_, next) ->
      let nodesAS = Impure.getAllNodes ()
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS
  | GetAllRelationships (_, next) ->
      let nodesAS = Impure.getAllRelationships ()
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS
  | GetNodesByChecksum (checksum, next) ->
      let nodesAS = Impure.getNodesByChecksum checksum
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS
  | GetNodesByLabel (label, next) ->
      let nodesAS = Impure.getNodesByLabel label
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS
  | GetPaths (input, next) ->
      let nodesAS = Impure.getPaths input
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS
  | GetPathsByNodeChecksum (input, next) ->
      let nodesAS = Impure.getPathsByNodeChecksum input
      let newProgramAS = (AsyncResult.map next) nodesAS
      interpret newProgramAS