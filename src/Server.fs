open Saturn
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Hobbes.Web.Routing
open Hobbes.Gateway.Services.Admin
open Hobbes.Gateway.Services.Data
open Hobbes.Gateway.Services.Root
open Hobbes.Gateway.Services.Derkins
open Hobbes.Helpers

open Hobbes.Web

let private port = 
    env "port" "8085" |> int
let private watch = 
        let w = System.Diagnostics.Stopwatch()
        w.Start()
        w   


let dataRouter = 
    router {
        pipe_through verifiedPipe
        withArg <@ json @> 
        //withBody <@ graphql @> 
    }

let adminRouter = 
   router {
        pipe_through verifiedPipe
        withBody <@ storeConfigurations @>
        fetch <@ getProjects @>
    }

let derkinsRouter = 
   router {
        pipe_through verifiedPipe
        withBody <@ identifyPii @>
    }

let private appRouter = router {
    not_found_handler (setStatusCode 404 >=> text "Api 404")
    
    fetch <@ ping @> 
    fetch <@ test @>
    withBody <@ key @>
    forward "/admin" adminRouter
    forward "/data" dataRouter
    forward "/derkins" derkinsRouter
} 

let private app = application {
    url (sprintf "http://0.0.0.0:%d/" port)
    use_router appRouter
    memory_cache
    use_gzip
}

let rec private init() =
    async {
        try
           FSharp.Data.Http.Request(Hobbes.Web.Database.ServerUrl) |> ignore //make sure db is up and running
           initDatabase() |> ignore
           printfn "DB initialized"
        with _ ->
           do! Async.Sleep 2000
           init()
    } |> Async.Start

let asm = System.Reflection.Assembly.GetExecutingAssembly() 
let asmName = asm.GetName()

let version = asmName.Version.ToString()      
printfn """{"appVersion": "%s", "name" : "%s"}""" version asmName.Name
init()
run app