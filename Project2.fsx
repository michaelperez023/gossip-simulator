// Author: Michael Perez UFID: 62408470
// Distributed Operating Systems - COP5615
#time "on"
#r "nuget: Akka.FSharp"

open System
open System.Diagnostics
open Akka.Actor
open Akka.FSharp
open System.Text
open System.Security.Cryptography

let BossActor (mailbox:Actor<_>) =

    let rec loop () = actor {
        let! message = mailbox.Receive()
        let sender = mailbox.Sender()

        match message with
        | BossInput(n) ->
        | _ -> ()
        return! loop ()
    }
    loop ()

match fsi.CommandLineArgs.Length with
| 2 ->
    let zeros = fsi.CommandLineArgs.[1] |> int

    let bossRef = spawn system "boss" BossActor
    bossRef <! BossInput(zeros)

    system.WhenTerminated.Wait()
    ()
| _ -> failwith "You must enter one number as input: i.e. 'dotnet fsi Project1.fsx 2'"