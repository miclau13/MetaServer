module Settings

open Thoth.Json.Net

type Settings = {
    PathBase : string
    NeoURL : string
    NeoUser : string
    NeoPassword : string
    BoltURL : string
}

let appsettings =
    let settings = System.IO.File.ReadAllText "appsettings.json"
    match Decode.Auto.fromString<Settings> (settings, caseStrategy=CamelCase) with
    | Ok s -> s
    | Error e -> failwith e

let pathBase = appsettings.PathBase
