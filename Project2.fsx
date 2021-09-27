// Authors: Blas Kojusner and Michael Perez UFID: 62408470
// Distributed Operating Systems - COP5615
#time "on"
#r "nuget: Akka.FSharp"

open System
open System.Diagnostics
open Akka.Actor
open Akka.FSharp

let system = ActorSystem.Create("System")
let timer = Stopwatch()

type Messages =
    | StartGossip of String
    | StartPushSum of String

let Boss (mailbox:Actor<_>) =
    let rec loop() = actor {
            let! message = mailbox.Receive()
            match message with

            | _->()

            return! loop()
        }
    loop()

let Node listener numNodes (mailbox:Actor<_>)  =
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
        | StartGossip msg ->
            printfn "starting gossip..."


        | StartPushSum msg ->
            printfn "starting push sum..."


        | _-> ()
        return! loop()
    }
    loop()

let start algo numNodes nodeArray =
            (nodeArray : _ array)|>ignore
            let r = System.Random()

            if algo = "gossip" then
                let starter = r.Next(0, numNodes-1)
                nodeArray.[starter] <! StartGossip("Hello")
            elif algo = "push-sum" then
                let starter = r.Next(0, numNodes-1)
                nodeArray.[starter] <! StartPushSum("Hello")
            else
                printfn "Wrong Algorithm Argument!"

let buildFull numNodes algo =
    printfn "Full network with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodeArray = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodeArray

let build3dGrid numNodes algo =
    printfn "3D grid with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodeArray = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodeArray

let buildLine numNodes algo =
    printfn "Line with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodeArray = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodeArray

let buildImp3d numNodes algo =
    printfn "Imperfect 3d grid build with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodeArray = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodeArray

match fsi.CommandLineArgs.Length with
| 4 ->
    let args : string array = fsi.CommandLineArgs |> Array.tail
    let numNodes = args.[0] |> int
    let topology = args.[1] |> string
    let algo = args.[2] |> string

    if topology="full" then buildFull numNodes algo
    elif topology="3D" then build3dGrid numNodes algo
    elif topology="line" then buildLine numNodes algo
    elif topology="imp3D" then buildImp3d numNodes algo
    else printfn "Wrong Topology Argument!"

    //System.Console.ReadLine() |> ignore

    //system.WhenTerminated.Wait()
    ()
| _ -> failwith "Error, run program with command: 'dotnet fsi Project2.fsx <numNodes> <topology> <algorithm>'"
