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
                neighbors.[r.Next(0, neighbors.Length)] <! StartGossip(rumor)

        | StartPushSum msg ->
            printfn "starting push sum..."
            
        | Neighbors neighbors_ ->
            neighbors <- neighbors_

        | _-> ()
        return! loop()
    }
    loop()

let start algo numNodes nodeArray =
    (nodeArray : _ array) |> ignore

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
    let mutable neighbors:IActorRef[] = Array.empty
            
    for i in [0..numNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))
    
    for i in [0..numNodes-1] do
        if i = 0 then
            neighbors <- nodes.[1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
        elif i = (numNodes-1) then 
            neighbors <- nodes.[0..(numNodes-2)]
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- Array.append nodes.[0..i-1] nodes.[i+1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
   
    timer.Start()
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, numNodes, nodes)
    start algo numNodes nodes

let build3dGrid numNodes algo =
    printfn "3D grid with %i nodes and algorithm %s" numNodes algo

    let boss = Boss |> spawn system "boss"
    let nodes = Array.zeroCreate(numNodes)
    let mutable neighbors:IActorRef[]=Array.empty
    
    for i in [0..numNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    let rowCount = int (ceil((float numNodes) ** (1.0/3.0))) // number of indices that correspond to a 1D row in the 3D grid
    let sliceCount = rowCount * rowCount // number of indices that correspond to a 2D slice of the 3D grid
    
    for i in [0..numNodes - 1] do
        if i = 0 then
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = rowCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount * (rowCount - 1) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount * (rowCount - 1) + rowCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = numNodes - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = numNodes - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i > 0 && i < rowCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount];|] // front face top row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount && i % rowCount = 0 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|] // front face left column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount && (i+1) % rowCount = 0 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|] // front face right column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|] // rest of front face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - rowCount && i < sliceCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount];|] // front face bottom row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > numNodes - rowCount && i < numNodes - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face bottom row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount];|] // back face top row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount && (i % rowCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face left column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount && ((i+1) % rowCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face right column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // rest of back face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) top left edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - rowCount + 1) % sliceCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) top right edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - ((rowCount - 1) * rowCount)) % sliceCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) bottom left edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % sliceCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) bottom right edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % rowCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) left face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % rowCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) right face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount < rowCount - 1) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) top face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount > sliceCount - rowCount) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // middle face(s) bottom face
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|] // inner nodes
            nodes.[i] <! Neighbors(neighbors)
        
    timer.Start()
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, numNodes, nodes)
    start algo numNodes nodes

let buildLine numNodes algo =
    printfn "Line with %i nodes and algorithm %s" numNodes algo
    
    let boss = Boss |> spawn system "boss"
    let nodes = Array.zeroCreate(numNodes)
    let mutable neighbors:IActorRef[]=Array.empty

    for i in [0..numNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    for i in [0..numNodes-1] do
        if i = 0 then
            neighbors <- [|nodes.[1]|]
            nodes.[i] <! Neighbors(neighbors)
        elif i = (numNodes-1) then 
            neighbors <- [|nodes.[numNodes-2]|]
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- [|nodes.[i-1]; nodes.[i+1]|]
            nodes.[i] <! Neighbors(neighbors)

    timer.Start()
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, numNodes, nodes)
    start algo numNodes nodes

let buildImp3d numNodes algo =
    printfn "Imperfect 3d grid build with %i nodes and algorithm %s" numNodes algo
    let boss = Boss |> spawn system "boss"
    let nodes = Array.zeroCreate(numNodes)
    let mutable neighbors:IActorRef[]=Array.empty

    for i in [0..numNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    let rowCount = int ((float numNodes) ** (1.0/3.0)) // number of indices that correspond to a 1D row in the 3D grid
    let sliceCount = int ((float numNodes) ** (2.0/3.0)) + 1 // number of indices that correspond to a 2D slice of the 3D grid

    for i in [0..numNodes - 1] do
        if i = 0 then
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = rowCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount * (rowCount - 1) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = sliceCount * (rowCount - 1) + rowCount - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = numNodes - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i = numNodes - 1 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
            nodes.[i] <! Neighbors(neighbors)
        elif i > 0 && i < rowCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // front face top row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount && i % rowCount = 0 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // front face left column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount && (i+1) % rowCount = 0 then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // front face right column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > rowCount - 1 && i < sliceCount - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // rest of front face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - rowCount && i < sliceCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]; nodes.[r.Next(0, nodes.Length)]|] // front face bottom row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > numNodes - rowCount && i < numNodes - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face bottom row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face top row edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount && (i % rowCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face left column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount && ((i+1) % rowCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face right column edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < numNodes - rowCount then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // rest of back face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) top left edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - rowCount + 1) % sliceCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) top right edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - ((rowCount - 1) * rowCount)) % sliceCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) bottom left edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % sliceCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) bottom right edge
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % rowCount = 0) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) left face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % rowCount = 0) then 
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) right face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount < rowCount - 1) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) top face
            nodes.[i] <! Neighbors(neighbors)
        elif i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount > sliceCount - rowCount) then 
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // middle face(s) bottom face
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // inner nodes
            nodes.[i] <! Neighbors(neighbors)

    timer.Start()
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, numNodes, nodes)
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
