module DbConfig

open System
open Neo4jClient
open Neo4j.Driver
open Settings

let getDbClient () =
    let boltUri = Uri(appsettings.BoltURL)
    let sandboxUserName = appsettings.NeoUser
    let sandboxUserPassword = appsettings.NeoPassword
    let driver = GraphDatabase.Driver(boltUri, AuthTokens.Basic(sandboxUserName, sandboxUserPassword));
    let client = new BoltGraphClient (driver);
    client.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously
    let clientWithCypher = client.Cypher
    clientWithCypher

