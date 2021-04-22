namespace Hobbes.Gateway.Services

open Hobbes.Web.Database
open Hobbes.Web.Log
open Hobbes.Web
open Hobbes.Web.Routing
open Hobbes.Helpers.Environment
open FSharp.Data
open Hobbes.FSharp.Compile
open Hobbes.Parsing
open Thoth.Json.Net

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

    type private ConfigPost = JsonProvider<"""{"name":"lkjlkj", "hb": "lkjlkjlkj"}""">

    [<Put ("/configuration", true)>]
    let storeConfigurations (doc : string) =
        let doc = ConfigPost.Parse doc
        let config = 
            let blocks = 
                let errors, blocks = BlockParser.parse doc.Hb
                if errors|> List.isEmpty |> not then
                    failwith (System.String.Join(",",errors))
                else
                    blocks

            let source,properties = 
                blocks
                |> List.map(function 
                     AST.Block.Source(source,properties) -> Some(source,properties)
                     | _ -> None
                ) |> List.find(Option.isSome)
                |> Option.get
            let rec encodeValue = 
                function
                    AST.Value.Mapping o -> 
                        Encode.object(o |> Map.toList |> List.map(fun (n,v) -> 
                            let name = 
                                match n with
                                AST.Value.String s -> s
                                | AST.Value.Boolean b -> string b
                                | AST.Value.Decimal d -> string d
                                | v -> failwithf "can't use %A as property name" v
                            name,encodeValue v
                        ))
                    | AST.Value.Sequence values ->
                        Encode.array (values |> List.map encodeValue |> Array.ofList)
                    | AST.Value.String s -> Encode.string s
                    | AST.Value.Boolean b -> Encode.bool b
                    | AST.Value.Decimal d -> Encode.decimal d
                    | AST.Value.Null -> Encode.nil

            let sourceConfig = 
                let properties = 
                    match properties |> Map.tryFind "meta" with
                    | None -> properties.Add("meta",Hobbes.Parsing.AST.Value.Mapping(Map.empty))
                    | Some _ -> properties
                let sourceObj = 
                    Encode.object 
                        (("provider", Encode.string source)::(
                             properties
                             |> Map.toList 
                             |> List.map(fun (n,v) ->
                                n, encodeValue v
                             )
                        ))
                let sourceHash = 
                   sourceObj
                   |> Encode.toString 0 
                   |> hash 

                let transformations = 
                    blocks
                    |> List.collect(function 
                         AST.Block.Statements stmts -> 
                             stmts
                             |> List.map(fun s -> 
                                 s.ToString()
                                 |> Encode.string
                             )
                         | _ -> []
                    ) |> Encode.list

                Encode.object [
                    "_id", Encode.string doc.Name
                    "sourceHash", Encode.string sourceHash
                    "source", sourceObj
                    "transformation", Encode.string doc.Hb
                ] |> Encode.toString 0
            
            sourceConfig

        let statusCode, msg =
            try
                match Http.post (None |> Http.Configuration |> Http.Configurations) config with
                Http.Success _ -> 200,sprintf """{"configuration":%s, "status" : "ok" }""" doc.Hb
                | Http.Error(s,m) -> 
                    assert(s > 100 && s < 600)
                    s,m
            with e -> 
                Log.excf e "Trying to store %s" doc.Hb
                500,sprintf "internal server error"
        assert(if statusCode > 100 && statusCode < 600 then eprintfn "Something is off %d %s" statusCode msg; false else true)
        if statusCode = 0 then failwithf "Can't return 0 %s" msg
        statusCode,msg

    [<Get ("/projects")>]
    let getProjects() =
        let auth = HttpRequestHeaders.BasicAuth (env "AZURE_DEVOPS_PAT" null) ""
        try
            let res1 = Http.Request("https://dev.azure.com/kmddk/_apis/projects?api-version=2.0",
                                     httpMethod = "GET",
                                     headers = [auth]
                                    )
            let res2 = Http.Request("https://dev.azure.com/time-payroll-kmddk/_apis/projects?api-version=2.0",
                                     httpMethod = "GET",
                                     headers = [auth]
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