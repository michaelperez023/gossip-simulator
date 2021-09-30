// Authors: Blas Kojusner and Michael Perez - UFID: 62408470 - Distributed Operating Systems - COP5615
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
    | GossipConverged of String
    | StartPushSum
    | PushSum of float * float
    | PushSumConverged of float * float
    | Neighbors of IActorRef[]
    | BossInit of int * int * IActorRef[]

let Boss (mailbox:Actor<_>) = 
    let mutable startTime = 0
    let mutable totalNodes = 0
    let mutable nodes:IActorRef[] = [||]
    let mutable nodeCount = 0

    let rec loop() = actor {

        let! message = mailbox.Receive()
        match message with 
            
        | GossipConverged message ->
            nodeCount <- nodeCount + 1
            if nodeCount = totalNodes then
                let rTime = timer.ElapsedMilliseconds
                let endTime = System.DateTime.Now.TimeOfDay.Milliseconds
                printfn "Convergence time from timer: %A ms" rTime
                printfn "Convergence time from System Time: %A ms" (endTime - startTime)
                Environment.Exit 0
            else
                nodes.[r.Next(0, nodes.Length)] <! StartGossip(message)
        
        | PushSumConverged (s, w) ->
            nodeCount <- nodeCount + 1
            if nodeCount = totalNodes then
                let rTime = timer.ElapsedMilliseconds
                let endTime = System.DateTime.Now.TimeOfDay.Milliseconds
                printfn "Convergence time from timer: %A ms" rTime
                printfn "Convergence time from System Time: %A ms" (endTime - startTime)
                Environment.Exit 0
            else
                nodes.[r.Next(0, nodes.Length)] <! PushSum(s, w)
        
        | BossInit (startTime_, totalNodes_, nodes_) ->
            startTime <- startTime_
            totalNodes <- totalNodes_
            nodes <- nodes_

        | _->()

        return! loop()
    }
    loop()

let Node boss numNodes (mailbox:Actor<_>)  =
    let mutable neighbors:IActorRef[] = [||]
    let mutable rumorsHeard = 0

    let mutable sum = numNodes |> float
    let mutable weight = 1.0
    let mutable roundCount = 1
    let mutable ratioRound1 = 1.0
    let mutable ratioRound2 = 1.0
    let mutable ratioRound3 = 1.0
    let mutable ratioRound4 = 1.0
    let mutable converged = false
    let mutable inFirstThreeRounds = true
    let ratioChangeThresh = 10.0 ** -10.0

    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
            | StartGossip rumor ->
                rumorsHeard <- rumorsHeard + 1

                if (rumorsHeard = 10) then
                    boss <! GossipConverged(rumor)
                else
                    neighbors.[r.Next(0, neighbors.Length)] <! StartGossip(rumor)

            | StartPushSum ->
                let temp = r.Next(0,neighbors.Length)
                neighbors.[temp] <! PushSum((temp |> float), 1.0)

            | PushSum (s,w) ->
                if converged then
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)

                if inFirstThreeRounds then
                    if roundCount = 1 then
                        ratioRound1 <- sum/weight
                    elif roundCount = 2 then
                        ratioRound2 <- sum/weight
                    elif roundCount = 3 then
                        ratioRound3 <- sum/weight
                        inFirstThreeRounds <- false
                
                sum <- (sum + s)/2.0
                weight <- (weight + w)/2.0
                ratioRound4 <- sum/weight

                if not inFirstThreeRounds then
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)
                
                if abs(ratioRound4 - ratioRound1) <= ratioChangeThresh && not converged then
                    converged <- true
                    boss <! PushSumConverged(sum, weight)
                else
                    ratioRound1 <- ratioRound2
                    ratioRound2 <- ratioRound3
                    ratioRound3 <- ratioRound4
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)

                roundCount <- roundCount + 1
                
            | Neighbors neighbors_ ->
                neighbors <- neighbors_

            | _-> ()
        return! loop()
    }
    loop()

let start algo numNodes nodeArray =
    (nodeArray : _ array) |> ignore

    if algo = "gossip" then
        nodeArray.[r.Next(0, numNodes-1)] <! StartGossip("Hello")
    elif algo = "push-sum" then
        nodeArray.[r.Next(0, numNodes-1)] <! StartPushSum
    else
        printfn "Error, wrong algorithm argument, it must be \"gossip\" or \"push-sum\""
        Environment.Exit 0

let buildFull numNodes algo =
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
    let boss = Boss |> spawn system "boss"

    let roundedNumNodes = int (float (int (ceil((float numNodes) ** (1.0/3.0)))) ** 3.0) // round number of nodes up to nearest perfect cube
    printfn "number of nodes: %i" roundedNumNodes

    let nodes = Array.zeroCreate(roundedNumNodes)
    let mutable neighbors:IActorRef[]=Array.empty

    for i in [0..roundedNumNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    let rowCount = int ((float roundedNumNodes) ** (1.0/3.0)) // number of indices that correspond to a 1D row in the 3D grid
    let sliceCount = rowCount * rowCount // number of indices that correspond to a 2D slice of the 3D grid

    if roundedNumNodes = 1 then
        printfn "Error, 3D grid can't be created with just one node, increase number of nodes"
        Environment.Exit 0
    else
        for i in [0..roundedNumNodes - 1] do
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
            elif i = roundedNumNodes - rowCount then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]|] // cube corner
                nodes.[i] <! Neighbors(neighbors)
            elif i = roundedNumNodes - 1 then 
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
            elif i > roundedNumNodes - rowCount && i < roundedNumNodes - 1 then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face bottom row edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount];|] // back face top row edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && (i % rowCount = 0) then 
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face left column edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && ((i+1) % rowCount = 0) then 
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|] // back face right column edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount then 
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
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, roundedNumNodes, nodes)
    start algo roundedNumNodes nodes

let buildLine numNodes algo =    
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
    let boss = Boss |> spawn system "boss"

    let roundedNumNodes = int (float (int (ceil((float numNodes) ** (1.0/3.0)))) ** 3.0) // round number of nodes up to nearest perfect cube
    printfn "number of nodes: %i" roundedNumNodes

    let nodes = Array.zeroCreate(roundedNumNodes)
    let mutable neighbors:IActorRef[]=Array.empty

    for i in [0..roundedNumNodes-1] do
        nodes.[i] <- Node boss (i+1) |> spawn system ("Node" + string(i))

    let rowCount = int ((float roundedNumNodes) ** (1.0/3.0)) // number of indices that correspond to a 1D row in the 3D grid
    let sliceCount = rowCount * rowCount // number of indices that correspond to a 2D slice of the 3D grid

    if roundedNumNodes = 1 then
        printfn "Error, imperfect 3D grid can't be created with just one node, increase number of nodes"
        Environment.Exit 0
    else
        for i in [0..roundedNumNodes - 1] do
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
            elif i = roundedNumNodes - rowCount then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // cube corner
                nodes.[i] <! Neighbors(neighbors)
            elif i = roundedNumNodes - 1 then 
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
            elif i > roundedNumNodes - rowCount && i < roundedNumNodes - 1 then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face bottom row edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 then 
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face top row edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && (i % rowCount = 0) then 
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face left column edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && ((i+1) % rowCount = 0) then 
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|] // back face right column edge
                nodes.[i] <! Neighbors(neighbors)
            elif i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount then 
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
    boss <! BossInit(System.DateTime.Now.TimeOfDay.Milliseconds, roundedNumNodes, nodes)
    start algo roundedNumNodes nodes

match fsi.CommandLineArgs.Length with
| 4 ->
    let args : string array = fsi.CommandLineArgs |> Array.tail
    let numNodes = args.[0] |> int
    let topology = args.[1] |> string
    let algo = args.[2] |> string

    if topology = "full" then buildFull numNodes algo
    elif topology = "3D" then build3dGrid numNodes algo
    elif topology = "line" then buildLine numNodes algo
    elif topology = "imp3D" then buildImp3d numNodes algo
    else
        printfn "Error, wrong topology argument, it must be \"full\", \"3D\", \"line\", or \"imp3D\""
        Environment.Exit 0

    //System.Console.ReadLine() |> ignore
    system.WhenTerminated.Wait()
    ()
| _ -> failwith "Error, wrong number of arguments, run program with command: 'dotnet fsi Project2.fsx <numNodes> <topology> <algorithm>'"
