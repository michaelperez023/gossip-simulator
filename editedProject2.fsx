// Authors: Blas Kojusner and Michael Perez - UFID: 62408470 - Distributed Operating Systems - COP5615
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
    | BossInit of int * IActorRef[]

let Boss (mailbox:Actor<_>) =
    let mutable totalNodes = 0
    let mutable nodes = [||]
    let mutable nodeCount = 0

    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
            | GossipConverged message ->
                nodeCount <- nodeCount + 1
                if nodeCount = totalNodes then
                    let rTime = timer.ElapsedMilliseconds
                    printfn "Convergence time from timer: %A ms" (double rTime)
                    Environment.Exit 0
                else
                    nodes.[r.Next(0, nodes.Length)] <! StartGossip(message)

            | PushSumConverged (s, w) ->
                nodeCount <- nodeCount + 1
                if nodeCount = totalNodes then
                    let rTime = timer.ElapsedMilliseconds
                    printfn "Convergence time from timer: %A ms" (double rTime)
                    Environment.Exit 0
                else
                    nodes.[r.Next(0, nodes.Length)] <! PushSum(s, w)

            | BossInit (totalNodes_, nodes_) ->
                totalNodes <- totalNodes_
                nodes <- nodes_

            | _-> ignore()
        return! loop()
    }
    loop()

let Node boss numNodes (mailbox:Actor<_>)  =
    let mutable rumorsHeard = 0
    let mutable neighbors = [||]
    let mutable gossip = true // to track whether the algo is gossip or push-sum
    let mutable receivedMessage = false

    let mutable rumor = ""
    let mutable sum = numNodes |> float
    let mutable weight = 1.0
    let mutable roundCount = 0
    let mutable ratios:double array = Array.zeroCreate 4
    let mutable converged = false
    let mutable inFirstThreeRounds = true
    let ratioChangeThresh = 10.0 ** -10.0

    //let mutable doneWithRumors = false

    let rec loop() = actor {
        let! message = mailbox.Receive()
        //if not doneWithRumors then
        match message with
            | StartGossip rumor_ ->
                receivedMessage <- true
                rumorsHeard <- rumorsHeard + 1
                rumor <- rumor_
                if (rumorsHeard = 10) then
                    boss <! GossipConverged(rumor_)
                else
                    neighbors.[r.Next(0, neighbors.Length)] <! StartGossip(rumor_)

            | StartPushSum ->
                gossip <- false
                receivedMessage <- true
                let temp = r.Next(0, neighbors.Length)
                neighbors.[temp] <! PushSum((temp |> float), 1.0)

            | PushSum (s,w)->
                receivedMessage <- true
                if converged then
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(s, w)

                if inFirstThreeRounds then
                    printfn "round count: %i" roundCount
                    ratios.[roundCount] <- sum/weight
                    if roundCount = 2 then
                        inFirstThreeRounds <- false
                    roundCount <- roundCount + 1

                sum <- (sum + s)/2.0
                weight <- (weight + w)/2.0
                ratios.[3] <- sum/weight

                if inFirstThreeRounds then
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)

                if abs(ratios.[3] - ratios.[0]) <= ratioChangeThresh && not converged then
                    converged <- true
                    boss <! PushSumConverged(sum, weight)
                else
                    for i in 0..2 do
                        ratios.[i] <- ratios.[i+1]
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)

            | Neighbors neighbors_ ->
                neighbors <- neighbors_

            | _-> ignore()

        (*if not receivedMessage then
            if gossip then
                if not (rumor.Equals("")) then
                    neighbors.[r.Next(0, neighbors.Length)] <! StartGossip(rumor)
            else
                if roundCount > 1 then
                    neighbors.[r.Next(0, neighbors.Length)] <! PushSum(sum, weight)
        receivedMessage <- false

        if receivedMessage then
            if rumorsHeard = 10 then
                doneWithRumors <- true*)

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
    // Instantiate variables for our full array
    let boss = Boss |> spawn system "boss"
    let mutable neighbors = Array.empty
    let nodes = [|for i in [0..numNodes-1] -> Node boss (i+1) |> spawn system ("Node" + string(i))|]

    // Fill out all the neighbors in our full array
    for i in [0..numNodes-1] do
        if i = 0 then
            neighbors <- nodes.[1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
        elif i <> (numNodes-1) then
            neighbors <- Array.append nodes.[0..i-1] nodes.[i+1..numNodes-1]
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- nodes.[0..(numNodes-2)]
            nodes.[i] <! Neighbors(neighbors)

    // Start the timer and return the finished array
    timer.Start()
    boss <! BossInit(numNodes, nodes)
    start algo numNodes nodes

let build3dGrid numNodes algo =
    let boss = Boss |> spawn system "boss"

    // Round number of nodes up to nearest perfect cube
    let roundedNumNodes = int(float(int(ceil((float numNodes) ** (1.0/3.0)))) ** 3.0)
    printfn "number of nodes: %i" roundedNumNodes
    // Get the number of indices that correspond to a 1D row in the 3D grid
    let rowCount = int (ceil((float numNodes) ** (1.0/3.0)))
    printfn "row count: %i" rowCount
    // Get the number of indices that correspond to a 2D slice of the 3D grid
    let sliceCount = rowCount * rowCount
    if roundedNumNodes = 1 then
        printfn "Error, 3D grid can't be created with just one node, increase number of nodes"
        Environment.Exit 0

    // Create our 3D array
    let mutable neighbors = Array.empty
    let nodes = [|for i in [0..roundedNumNodes-1] -> Node boss (i+1) |> spawn system ("Node" + string(i))|]

    for i in [0..roundedNumNodes - 1] do
        match i with
            | i when i = 0 ->
                // Cube corner #1
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = rowCount - 1 ->
                // Cube corner #2
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = sliceCount - rowCount ->
                // Cube corner #3
                neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = sliceCount - 1 ->
                // Cube corner #4
                neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = sliceCount * (rowCount - 1) ->
                // Cube corner #5
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = sliceCount * (rowCount - 1) + rowCount - 1 ->
                // Cube corner #6
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = roundedNumNodes - rowCount ->
                // Cube corner #7
                neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i = roundedNumNodes - 1 ->
                // Cube corner #8
                neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > 0 && i < rowCount - 1 ->
                // Front face top row edge
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > rowCount - 1 && i < sliceCount - rowCount && i % rowCount = 0 ->
                // Front face left column edge
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > rowCount - 1 && i < sliceCount - rowCount && (i+1) % rowCount = 0 ->
                // Front face right column edge
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > rowCount - 1 && i < sliceCount - rowCount ->
                // Front face remainder
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - rowCount && i < sliceCount - 1 ->
                // Front face bottom row edge
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > roundedNumNodes - rowCount && i < roundedNumNodes - 1 ->
                // Back face bottom row edge
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 ->
                // Back face top row edge
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && (i % rowCount = 0) ->
                // Back face left column edge
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && ((i+1) % rowCount = 0) ->
                // Back face right column edge
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount ->
                // Back face remainder
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount = 0) ->
                // Middle face(s) top left edge
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - rowCount + 1) % sliceCount = 0) ->
                // Middle face(s) top right edge
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - ((rowCount - 1) * rowCount)) % sliceCount = 0) ->
                // Middle face(s) bottom left edge
                neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % sliceCount = 0) ->
                // Middle face(s) bottom right edge
                neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % rowCount = 0) ->
                // Middle face(s) left face
                neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % rowCount = 0) ->
                // Middle face(s) right face
                neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount < rowCount - 1) ->
                // Middle face(s) top face
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount > sliceCount - rowCount) ->
                // Middle face(s) bottom face
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)
            | _ ->
                // Inner nodes
                neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount];|]
                nodes.[i] <! Neighbors(neighbors)

    // Start the timer and return the 3D array
    timer.Start()
    boss <! BossInit(roundedNumNodes, nodes)
    start algo roundedNumNodes nodes

let buildLine numNodes algo =
    // Instantiate variables for our line
    let boss = Boss |> spawn system "boss"
    let mutable neighbors = Array.empty
    let nodes = [|for i in [0..numNodes-1] -> Node boss (i+1) |> spawn system ("Node" + string(i))|]

    // Fill out all the neighbors in our line
    for i in [0..numNodes-1] do
        if i = 0 then
            neighbors <- [|nodes.[1]|]
            nodes.[i] <! Neighbors(neighbors)
        elif i <> (numNodes-1) then
            neighbors <- [|nodes.[i-1]; nodes.[i+1]|]
            nodes.[i] <! Neighbors(neighbors)
        else
            neighbors <- [|nodes.[numNodes-2]|]
            nodes.[i] <! Neighbors(neighbors)

    // Start the timer and return the finished array
    timer.Start()
    boss <! BossInit(numNodes, nodes)
    start algo numNodes nodes

let buildImp3d numNodes algo =
    let boss = Boss |> spawn system "boss"

    // Round number of nodes up to nearest perfect cube
    let roundedNumNodes = int (float (int (ceil((float numNodes) ** (1.0/3.0)))) ** 3.0)
    printfn "number of nodes: %i" roundedNumNodes
    // Get the number of indices that correspond to a 1D row in the 3D grid
    let rowCount = int ((float roundedNumNodes) ** (1.0/3.0))
    // Get the number of indices that correspond to a 2D slice of the 3D grid
    let sliceCount = rowCount * rowCount
    if roundedNumNodes = 1 then
        printfn "Error, imperfect 3D grid can't be created with just one node, increase number of nodes"
        Environment.Exit 0

    // Create our Imp3D array
    let mutable neighbors:IActorRef[] = Array.empty
    let nodes = [|for i in [0..roundedNumNodes-1] -> Node boss (i+1) |> spawn system ("Node" + string(i))|]

    for i in [0..roundedNumNodes - 1] do
        match i with
        | i when i = 0 ->
            // Cube corner
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = rowCount - 1 ->
            // Cube corner
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = sliceCount - rowCount ->
            // Cube corner
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = sliceCount - 1 ->
            // Cube corner
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = sliceCount * (rowCount - 1) ->
            // Cube corner
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = sliceCount * (rowCount - 1) + rowCount - 1 ->
            // Cube corner
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = roundedNumNodes - rowCount ->
            // Cube corner
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i = roundedNumNodes - 1 ->
            // Cube corner
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > 0 && i < rowCount - 1 ->
            // Front face top row edge
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > rowCount - 1 && i < sliceCount - rowCount && i % rowCount = 0 ->
            // Front face left column edge
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > rowCount - 1 && i < sliceCount - rowCount && (i+1) % rowCount = 0 ->
            // Front face right column edge
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > rowCount - 1 && i < sliceCount - rowCount ->
            // Rest of front face
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - rowCount && i < sliceCount - 1 ->
            // Front face bottom row edge
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[r.Next(0, nodes.Length)]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > roundedNumNodes - rowCount && i < roundedNumNodes - 1 ->
            // Back face bottom row edge
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount * (rowCount - 1) && i < sliceCount * (rowCount - 1) + rowCount - 1 ->
            // Back face top row edge
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && (i % rowCount = 0) ->
            // Back face left column edge
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount && ((i+1) % rowCount = 0) ->
            // Back face right column edge
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount * (rowCount - 1) + rowCount - 1 && i < roundedNumNodes - rowCount ->
            // Rest of back face
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount = 0) ->
            // Middle face(s) top left edge
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - rowCount + 1) % sliceCount = 0) ->
            // Middle face(s) top right edge
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i - ((rowCount - 1) * rowCount)) % sliceCount = 0) ->
            // Middle face(s) bottom left edge
            neighbors <- [|nodes.[i + 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % sliceCount = 0) ->
            // Middle face(s) bottom right edge
            neighbors <- [|nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % rowCount = 0) ->
            // Middle face(s) left face
            neighbors <- [|nodes.[i + 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && ((i + 1) % rowCount = 0) ->
            // Middle face(s) right face
            neighbors <- [|nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount < rowCount - 1) ->
            // Middle face(s) top face
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | i when i > sliceCount - 1 && i < sliceCount * (rowCount - 1) && (i % sliceCount > sliceCount - rowCount) ->
            // Middle face(s) bottom face
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)
        | _ ->
            // Inner nodes
            neighbors <- [|nodes.[i + 1]; nodes.[i - 1]; nodes.[i - rowCount]; nodes.[i + rowCount]; nodes.[i + sliceCount]; nodes.[i - sliceCount]; nodes.[r.Next(0, nodes.Length)]|]
            nodes.[i] <! Neighbors(neighbors)

    // Start the timer and return the 3D array
    timer.Start()
    boss <! BossInit(roundedNumNodes, nodes)
    start algo roundedNumNodes nodes

match fsi.CommandLineArgs.Length with
| 4 ->
    let numNodes = fsi.CommandLineArgs.[1] |> int
    let algo = fsi.CommandLineArgs.[3]

    // Fetch topology requested
    match fsi.CommandLineArgs.[2] with
    | "full" ->
        buildFull numNodes algo
    | "3D" ->
        build3dGrid numNodes algo
    | "line" ->
        buildLine numNodes algo
    | "imp3D" ->
        buildImp3d numNodes algo
    | _ ->
        printfn "Error, wrong topology argument, it must be \"line\", \"full\", \"3D\", or \"imp3D\""
        exit 0

    system.WhenTerminated.Wait()
    ()
| _ -> failwith "Error, wrong number of arguments, run program with command: 'dotnet fsi Project2.fsx <numNodes> <topology> <algorithm>'"
