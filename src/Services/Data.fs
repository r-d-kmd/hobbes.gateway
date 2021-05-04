namespace Hobbes.Gateway.Services

open Hobbes.Web.Log
open Hobbes.Web.Routing
open Hobbes.Helpers.Environment
open Hobbes.Web
open Hobbes.Messaging.Broker


[<RouteArea "/data">]
module Data = 
    type private Data = FSharp.Data.JsonProvider<"""
        {
            "_id":"Velocity",
            "_rev":"1-828b3bba2434713d6e4bfced57b03c99",
            "data":[
                {
                    "Velocity 3":null,
                    "Velocity 7":null
                }
             ],
             "name":"Velocity",
             "meta":{
                "category":"workitems",
                "name":"flowerpot"
            }
        }""">
    let private cacheRevision confDoc = 
        sprintf "%s:%d" confDoc (System.DateTime.Now.Ticks) |> hash

    [<Get ("/json/%s")>]
    let json configuration =  
        debugf "Getting json for '%A'" configuration
        match Http.get (configuration |> Some |> Http.Configuration |> Http.Configurations) RawdataTypes.Config.Parse with
        Http.Success config -> 
            match Http.get (config.Id |> Http.UniformDataService.Read |> Http.UniformData) id with
            Http.Success json ->
                200, json
            | Http.Error(sc,m) ->
                sc,sprintf "Failed retrieving data. Message: %s" m
        | Http.Error(sc,m) -> sc,sprintf "Configuration %s not found. Message: %s" configuration m
    