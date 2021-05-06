namespace Hobbes.Gateway.Services

open Hobbes.Web.Log
open Hobbes.Web.Routing
open Hobbes.Helpers.Environment
open Hobbes.Web
open Hobbes.Messaging.Broker
open FSharp.Data

[<RouteArea "/derkins">]
module Derkins = 
    [<Post ("/identifypii", true)>]
    let identifyPii (doc : string) =
        let resp = 
            Http.Request("http://derkins-svc:8085/identifypii",
                httpMethod = "POST",
                silentHttpErrors = false,
                body = HttpRequestBody.TextRequest doc
            )

        let body =
            match resp.Body with
            | Binary b -> 
                let enc = 
                    match resp.Headers |> Map.tryFind "Content-Type" with
                    None -> System.Text.Encoding.Default
                    | Some s ->
                        s.Split "=" 
                        |> Array.last
                        |> System.Text.Encoding.GetEncoding 
                enc.GetString b
            | Text t -> t
            
        resp.StatusCode, body
        