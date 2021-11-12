module Db

open System
open Neo4jClient
open Neo4j.Driver
open Settings

[<CLIMutable>]
type File = {
    Path: string
    Name: string
    Format: string
    Checksum: string
}

type Grid = {
    Checksum: string
    NodeNumber: int
}

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

let getDbClient () =
    let boltUri = Uri(appsettings.BoltURL)
    let sandboxUserName = appsettings.NeoUser
    let sandboxUserPassword = appsettings.NeoPassword
    let driver = GraphDatabase.Driver(boltUri, AuthTokens.Basic(sandboxUserName, sandboxUserPassword));
    let client = new BoltGraphClient (driver);
    client.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
    let clientWithCypher = client.Cypher
    clientWithCypher

