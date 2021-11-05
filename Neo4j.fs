module Neo4j
open System
open Neo4jClient
open Neo4j.Driver

[<CLIMutable>]
type File = {
    Path: string
    Name: string
    Extension: string
    Checksum: string
}

type Grid = {
    Checksum: string
    NodeNumber: int
    CellNumber: int
}

type River = {
    Checksum: string
    RiverName: string
    RiverGridLocation: int
    RiverVerticalDistribution: list<float>
    RiverFile: string
}

type Node =
    | File of File

let sandboxUri = Uri("http://3.239.27.71:7474")
let boltUri = Uri("neo4j://localhost:7687")
let sandboxUserName = "neo4j"
let sandboxUserPassword = "abc"
let driver = GraphDatabase.Driver(boltUri, AuthTokens.Basic(sandboxUserName, sandboxUserPassword));
let client = new BoltGraphClient (driver);
client.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

// let client = new GraphClient(sandboxUri, sandboxUserName, sandboxUserPassword);
// client.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
// let session = driver.AsyncSession(o => o.WithDatabase("neo4j"));
let clientWithCypher = client.Cypher

let runNML = {
    Path = "/work/jonas/Ti1"
    Name = "Ti1_run"
    Extension = "nml"
    Checksum = "883caa0d769cf15d52a25916105db81152fd5a4e78642a7f39d57e13421bf7e4"
}

let riverNamelistNML = {
    Path = "/work/jonas/Ti1"
    Name = "RiverNamelist"
    Extension = "nml"
    Checksum = "1942d76f855b979531bae445451e1ffbd8b168ea0f01f1d1511c38e23c678d66"
}

let river1 = {
    RiverName = "026.Z - SIRA _ 02"
    Checksum = "xab"
    RiverGridLocation = 435
    RiverVerticalDistribution = [0.00177;0.00210]
    RiverFile = "riverdata.nc"
}

let files = [runNML; riverNamelistNML]
let fileLabels = ["File"]

let getNodeLabel (node: Node) =
    let label =
        match node with
        | File _ -> nameof File
    label

let create (node: Node) =

    let cypherCreate (node: Node) =
        try
            let label = getNodeLabel node
            let query = sprintf "(n:%s $node)" label
            let client' = clientWithCypher.Create(query)
            let client'' =
                match node with
                | File file -> client'.WithParam("node", file)
            client''
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

let createMultipleNodes props (labels: list<string>) =
    let convertLabelsToQuery acc label =
        sprintf "%s:%s" acc label
    let labelString = List.fold convertLabelsToQuery "" labels
    let nodeLabel = sprintf "(n%s)" labelString
    try
        let client' =
            clientWithCypher
                .WithParam("props", props)
                .Unwind ("$props", "nodes")
        client'
            .Create(nodeLabel)
            .Set("n = nodes")
            .ExecuteWithoutResultsAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

        let message = sprintf "%A are created successfully!" props
        Ok(props, message)
    with
    | error ->
        let message = sprintf "Exception in creating node: %s" error.Message
        Error(message)

let update (node: Node) =

    let cypherUpdate (node: Node) =
        try
            let label = getNodeLabel node
            let query = sprintf "(n:%s)" label
            let client' = clientWithCypher.Match(query)
            let client'' =
                match node with
                | File file -> client'.WithParam("node", file)
            client''
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

let createFileIfNotExist (file: File) =
    client
        .Cypher
        .Merge("(file:File {Checksum: $newFile.Checksum})")
        .OnCreate()
        .Set("file = $newFile")
        .WithParam("newFile", file)
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

// let updateFile (newFile: File) =
//     client
//         .Cypher
//         .Match("(file:File)")
//         .Where(fun (file: File) -> file.Name = newFile.Name)
//         .Set("file.Id = $newUser.Id")
//         .WithParam("newUser", newUser)
//         .ExecuteWithoutResultsAsync()
//     |> Async.AwaitTask |> Async.RunSynchronously

let getAllFile () =
    let result =
        client
            .Cypher
            .Match("(file:File)")
            .Return(fun (file : Cypher.ICypherResultItem) -> file.As())
            .ResultsAsync
    let a =
        result |> Async.AwaitTask |> Async.RunSynchronously
    for i in a do
        printfn "ans: %s" i
    result

// let deleteFile (file': File) =
//     client
//         .Cypher
//         .Match("(file:File)")
//         .Where(fun (file: File) -> file.Checksum = file'.Checksum)
//         .Delete("file")
//         .ExecuteWithoutResultsAsync()
//     |> Async.AwaitTask |> Async.RunSynchronously

let deleteAllFiles =
    client
        .Cypher
        .Match("(file:File)")
        .Delete("file")
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let relateInputFile (sourceFile': File) (targetFile': File) =
    client
        .Cypher
        .Match("(sourceFile:File)" , "(targetFile:File)")
        .Where(fun (sourceFile: File) -> sourceFile.Checksum = sourceFile'.Checksum)
        .AndWhere(fun (targetFile: File) -> targetFile.Checksum = targetFile'.Checksum)
        .Create("(sourceFile)-[:HAS_INPUT]->(targetFile)")
        .ExecuteWithoutResultsAsync()
    |> Async.AwaitTask |> Async.RunSynchronously

let createMultipleFiles () = createMultipleNodes files fileLabels
let createIfNotExist () = createFileIfNotExist riverNamelistNML
let relateFile () = relateInputFile runNML riverNamelistNML
// let updateMiclo () = updateUser jim
// let deleteMiclolo () = deleteUser miclolo
// let relateMicloFriendsWithMiclolo () = relateUser miclo miclolo