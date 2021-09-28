// Authors: Blas Kojusner and Michael Perez UFID: 62408470
// Distributed Operating Systems - COP5615
#time "on"
#r "nuget: Akka.FSharp"

open System
open Akka.Actor
open Akka.FSharp

let system = ActorSystem.Create("System")
let timer = System.Diagnostics.Stopwatch()
let r = System.Random()

type Messages =
    | StartGossip of String
    | StartPushSum of String
    | Neighbors of IActorRef[]
    | BossInit of int * int * IActorRef[]
    | GossipConverged of String
    | PushSumConverged of float * float

let Boss (mailbox:Actor<_>) = 
    let mutable startTime = 0
    let mutable numNodes = 0
    let mutable nodes:IActorRef[] = [||]
    let mutable numRumors = 0

    let rec loop() = actor {

        let! message = mailbox.Receive()
        match message with 
            
        | GossipConverged message ->
            numRumors <- numRumors + 1
            if numRumors = numNodes then
                let rTime = timer.ElapsedMilliseconds
                let endTime = System.DateTime.Now.TimeOfDay.Milliseconds
                printfn "Convergence time from timer: %A ms" rTime
                printfn "Convergence time from System Time: %A ms" (endTime - startTime)
                Environment.Exit 0
            else
                let index = r.Next(0, nodes.Length)
                nodes.[index] <! StartGossip("Hello")
        
        | BossInit (startTime_, numNodes_, nodes_) ->
            startTime <- startTime_
            numNodes <- numNodes_
            nodes <- nodes_

        | _->()

        return! loop()
    }
    loop()

let Node boss numNodes (mailbox:Actor<_>)  =
    let mutable neighbors:IActorRef[] = [||]
    let mutable rumorsHeard = 0

    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
        | StartGossip rumor ->
            rumorsHeard <- rumorsHeard + 1

            if (rumorsHeard = 10) then
                boss <! GossipConverged(rumor)
            else
                let index= r.Next(0, neighbors.Length)
                neighbors.[index] <! StartGossip(rumor)

        | StartPushSum msg ->
            printfn "starting push sum..."
            
        | Neighbors neighbors_ ->
            neighbors<-neighbors_

        | _-> ()
        return! loop()
    }
    loop()

let start algo numNodes nodeArray =
    (nodeArray : _ array)|>ignore

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

    let nodes = Array.zeroCreate(numNodes)

    let mutable neighbors:IActorRef[]=Array.empty
            
    for i in [0..numNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))
    
    for i in [0..numNodes-1] do
        if i=0 then
            neighbors<-nodes.[1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
        elif i=(numNodes-1) then 
            neighbors<-nodes.[0..(numNodes-2)]
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors<-Array.append nodes.[0..i-1] nodes.[i+1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
   
    timer.Start()
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, numNodes, nodes)

    start algo numNodes nodes

let build3dGrid numNodes algo =
    printfn "3D grid with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"

    let nodes = Array.zeroCreate(numNodes)
    
    //for i in [0..numNodes-1] do
    //    nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    timer.Start()
    start algo numNodes nodes

let buildLine numNodes algo =
    printfn "Line with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodes = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodes

let buildImp3d numNodes algo =
    printfn "Imperfect 3d grid build with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodes = Array.zeroCreate(numNodes)

    timer.Start()
    start algo numNodes nodes

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

    system.WhenTerminated.Wait()
    ()
| _ -> failwith "Error, run program with command: 'dotnet fsi Project2.fsx <numNodes> <topology> <algorithm>'"
