namespace Hobbes.Gateway.Services

open Hobbes.Web.Log
open Hobbes.Web.Routing
open Hobbes.Helpers.Environment
open Hobbes.Web
open Hobbes.Messaging.Broker
[<RouteArea "/data">]
module Data = 
    let private cacheRevision confDoc = 
        sprintf "%s:%d" confDoc (System.DateTime.Now.Ticks) |> hash

    [<Get ("/json/%s")>]
    let json configuration =  
        debugf "Getting json for '%A'" configuration
        match Http.get (configuration |> Some |> Http.Configuration |> Http.Configurations) RawdataTypes.Config.Parse with
        Http.Success config -> 
            let key = config.Name
            match Http.get (key |> Http.UniformDataService.Read |> Http.UniformData) id with
            Http.Success json ->
                200, json
            | Http.Error(sc,m) ->
                sc,sprintf "Failed retrieving data. Message: %s" m
        | Http.Error(sc,m) -> sc,sprintf "Configuration %s not found. Message: %s" configuration m
    