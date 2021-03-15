namespace Hobbes.Gateway.Services

open Hobbes.Web.Database
open Hobbes.Web.Log
open Hobbes.Web.Routing
open Hobbes.Web.Security
open Hobbes.Helpers.Environment

[<RouteArea ("/", false)>]
module Root =
    
    [<Get "/">]
    let test() = 
        200, "test"
    
    [<Get "/ping" >] 
    let ping() = 
        let app = Microsoft.Extensions.PlatformAbstractions.PlatformServices.Default.Application
        200,sprintf """{"Appversion": "%s", "runtimeFramework" : "%s", "appName" : "%s"}""" app.ApplicationVersion app.RuntimeFramework.FullName app.ApplicationName
    
    type UserSpec = FSharp.Data.JsonProvider<"""{"name" : "kjlkj", "token" : "elkælk"}""">

    [<Put ("/key", true) >] 
    let key userStr =
        debugf "User: %s" userStr
        let user = userStr |> UserSpec.Parse
        let verifiedUser = 
            let userIds = 
                users.ListIds() 
                |> Seq.filter(fun userId -> userId.StartsWith "org.couchdb.user:") 
                |> List.ofSeq
                
            debugf "Users in system: %A" userIds
            let isFirstUser = userIds |> List.isEmpty && System.String.IsNullOrWhiteSpace(user.Token)
            if isFirstUser ||  verifyAuthToken user.Token then
                let token = 
                   let rndstring = randomString 16
                   ( env "KEY_SUFFIX" (randomString 16)) + user.Name + rndstring |> hash
                
                let userId = sprintf "org.couchdb.user:%s" user.Name
                match users.TryGet userId with
                None  ->
                  logf  "Didn't find user. %s" userId
                  let userRecord = 
                      sprintf """{
                          "_id" : "%s",
                        "name": "%s",
                        "type": "user",
                        "roles": [],
                        "password": "%s"
                      }""" userId user.Name token

                  userRecord
                  |> users.InsertOrUpdate
                  |> ignore
                  users.FilterByKeys [userId]
                  |> Seq.head
                  |> Some
                | s -> s 
            else
                None        

        match verifiedUser with
        None ->
            errorf "No user token. Tried with %s" user.Token 
            403,"Unauthorized"
        | Some (user) ->
            printfn "Creating api key for %s " user.Name
            let key = createToken user
            200,key
