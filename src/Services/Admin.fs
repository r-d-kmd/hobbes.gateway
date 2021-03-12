namespace Hobbes.Gateway.Services

open Hobbes.Web.Database
open Hobbes.Web.Log
open Hobbes.Web
open Hobbes.Web.Routing
open Hobbes.Helpers.Environment
open FSharp.Data


[<RouteArea "/admin">]
module Admin = 

    type Projects = JsonProvider<"""{
        "count": 53,
        "value": [
            {
                "id": "81c5e879-ac46-40c7-88d4-62a531864ad1",
                "name": "NEXUS",
                "url": "https://dev.azure.com/kmddk/_apis/projects/81c5e879-ac46-40c7-88d4-62a531864ad1",
                "state": "wellFormed",
                "revision": 2588,
                "visibility": "private",
                "lastUpdateTime": "2020-06-03T11:53:18.177Z"
            }
        ]
    }""">


    [<Put ("/transformation",true)>]
    let storeTransformations doc =
        try
            match Http.post (None |> Http.Transformation |> Http.Configurations) doc with
            Http.Success _ -> 200,sprintf """{"transformation":%s, "status" : "ok" }""" doc
            | Http.Error(s,m) -> s,sprintf "message: %A, doc: %A" m doc
        with e -> 
            Log.excf e "Trying to store %s" doc
            500,sprintf "internal server error"

    [<Put ("/configuration",true)>]
    let storeConfigurations doc = 
        try
            match Http.post (None |> Http.Configuration |> Http.Configurations) doc with
            Http.Success _ -> 200,sprintf """{"configuration":%s, "status" : "ok" }""" doc
            | Http.Error(s,m) -> s,m
        with e -> 
            Log.excf e "Trying to store %s" doc
            500,sprintf "internal server error"

    [<Get ("/projects")>]
    let getProjects() =
        try
            let res1 = Http.Request("https://dev.azure.com/kmddk/_apis/projects?api-version=2.0",
                                     httpMethod = "GET",
                                     headers = [HttpRequestHeaders.BasicAuth (env "AZURE_TOKEN_KMDDK" null) ""]
                                    )
            let res2 = Http.Request("https://dev.azure.com/time-payroll-kmddk/_apis/projects?api-version=2.0",
                                     httpMethod = "GET",
                                     headers = [HttpRequestHeaders.BasicAuth (env "AZURE_TOKEN_TIME_PAYROLL_KMDDK" null) ""]
                                   )

            let getNames resBody =
                (resBody
                |> Http.readBody
                |> Projects.Parse).Value
                |> Array.map (fun (j : Projects.Value) -> sprintf "%A" j.Name)
                |> List.ofArray

            200, getNames res1@getNames res2
                 |> String.concat ","
                 |> sprintf "[%s]" 
        with e ->
            Log.exc e "Trying to get projects from azure"
            500, sprintf "internal server error"
    
    let private uploadDesignDocument (db : Database<CouchDoc.Root>, file) =
        
        async {
            let! doc = System.IO.File.ReadAllTextAsync file |> Async.AwaitTask
            if System.String.IsNullOrWhiteSpace (CouchDoc.Parse doc).Rev |> not then failwithf "Initialization documents shouldn't have _revs %s" file
            let designDocName = System.IO.Path.GetFileNameWithoutExtension file
            let oldHash = designDocName
                          |> db.TryGetHash
            let newDoc = (doc 
                           |> String.filter(not << System.Char.IsWhiteSpace))
                           
            let newHash = hash newDoc                

            let res = 
                if oldHash.IsNone || oldHash.Value <> newHash then
                    let id = sprintf "%s_hash" designDocName
                    sprintf """{"_id": %A, "hash":%A }"""  id newHash
                    |> db.InsertOrUpdate 
                    |> ignore
                    db.InsertOrUpdate doc
                else 
                    ""
            db.CompactAndClean()
            return res
        }
        
    let initDatabase () =
        awaitDbServer()
        let systemDbs = 
            [
                "log"
            ]
        let errorCode = 
            systemDbs
            |> List.map (fun n -> couch.TryPut(n, "") |> fst)
            |> List.tryFind (fun sc -> ((sc >= 200 && sc < 300) || (sc = 412)) |> not)
        match errorCode with
         Some errorCode ->
            let msg = sprintf "INIT: error in creating dbs. Status code: %d" errorCode
            error msg
         | None ->
            let msg = "Init completed"
            log msg