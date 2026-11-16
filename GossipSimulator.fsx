// Gossip and push-sum on four network topologies, with one Akka.NET actor per node and synchronous rounds.
// Authors: Blas Kojusner and Michael Perez - Distributed Operating Systems - COP5615
//
// Usage: dotnet fsi GossipSimulator.fsx <numNodes> <topology> <algorithm> [options]
//   numNodes            2 to 100,000,000; 3D and imp3D round it up to the next perfect cube
//   topology            full, 3D, line or imp3D
//   algorithm           gossip or push-sum
//   --seed <n>          seed for every random choice (-2147483648 to 2147483647); the same seed gives the same
//                       rounds, messages and estimates
//   --verify            check the topology and the result (exit code 1 if a check fails)
//   --max-rounds <n>    stop after n rounds if the run has not converged (exit code 1); see defaultMaxRounds
//   --rumor-limit <n>   gossip: a node stops sending once it has heard the rumor n times (default 10)
//   --csv               print one line of comma-separated values instead of the report
// "dotnet fsi GossipSimulator.fsx --csv-header" prints the names of the CSV columns.

#r "nuget: Akka.FSharp, 1.5.74"

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Numerics
open System.Threading.Tasks
open Akka.Actor
open Akka.FSharp

// ---------------------------------------------------------------------------
// Random numbers

/// The output function of SplitMix64: mixes the bits of a 64-bit word.
let mix64 (x: uint64) =
    let z = (x ^^^ (x >>> 30)) * 0xBF58476D1CE4E5B9UL
    let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
    z ^^^ (z >>> 31)

/// SplitMix64 (Steele, Lea and Flood, OOPSLA 2014). It keeps 8 bytes of state and gives the same numbers on every
/// platform. Each node has its own generator, so a run does not depend on which thread runs which actor when.
type Rng(seed: uint64) =
    let mutable state = seed

    member _.NextUInt64() =
        state <- state + 0x9E3779B97F4A7C15UL
        mix64 state

    /// A uniform integer in [0, n) for n >= 1. Draws below 2^64 mod n are rejected, so there is no modulo bias.
    member this.Next(n: int) =
        let bound = uint64 n
        let threshold = (0UL - bound) % bound
        let mutable x = this.NextUInt64()

        while x < threshold do
            x <- this.NextUInt64()

        int (x % bound)

/// Separate streams of random numbers, so that, for example, the imp3D links do not shift the nodes' choices.
type Stream =
    | NodeChoices = 1
    | ExtraLinks = 2
    | StartNode = 3

/// The generator for one stream and index under a seed.
let generator (seed: int) (stream: Stream) (index: int) =
    Rng(mix64 (mix64 (mix64 (uint64 (uint32 seed)) + uint64 (int stream)) + uint64 index))

// ---------------------------------------------------------------------------
// Topologies

type Shape =
    | Full
    | Grid3D
    | Line
    | Imperfect3D

type Topology =
    { Shape: Shape
      Nodes: int
      /// k for the grids, which have k x k x k nodes; 0 for full and line
      Side: int
      /// The neighbors of node i are Targets.[Offsets.[i] .. Offsets.[i + 1] - 1]. In imp3D the last of them is the
      /// extra random neighbor. A full network has no lists: a node there picks one of the other n - 1 nodes.
      Offsets: int[]
      Targets: int[] }

    member t.Degree i = t.Offsets.[i + 1] - t.Offsets.[i]

/// The smallest k with k^3 >= n: a floating-point estimate, corrected in integer arithmetic.
let cubeSide (n: int) =
    let cube (k: int) = int64 k * int64 k * int64 k
    let mutable k = max 1 (int (Math.Round(Math.Cbrt(float n))))

    while cube k < int64 n do
        k <- k + 1

    while k > 1 && cube (k - 1) >= int64 n do
        k <- k - 1

    k

/// Node i of a k x k x k grid sits at (x, y, z) with i = x + k*y + k*k*z.
let coordinates (k: int) (i: int) = struct (i % k, (i / k) % k, i / (k * k))

/// The grid neighbors of node i: one step along each axis, where that stays inside the cube.
let gridNeighbors (k: int) (i: int) =
    let struct (x, y, z) = coordinates k i

    [| if x > 0 then i - 1
       if x < k - 1 then i + 1
       if y > 0 then i - k
       if y < k - 1 then i + k
       if z > 0 then i - k * k
       if z < k - 1 then i + k * k |]

let buildTopology (shape: Shape) (requested: int) (seed: int) =
    let fromLists (nodes: int) (side: int) (neighbors: int -> int[]) =
        let offsets = Array.zeroCreate (nodes + 1)
        let targets = ResizeArray<int>()

        for i in 0 .. nodes - 1 do
            targets.AddRange(neighbors i)
            offsets.[i + 1] <- targets.Count

        ({ Shape = shape
           Nodes = nodes
           Side = side
           Offsets = offsets
           Targets = targets.ToArray() }: Topology)

    match shape with
    | Full ->
        ({ Shape = Full
           Nodes = requested
           Side = 0
           Offsets = [||]
           Targets = [||] }: Topology)
    | Line ->
        fromLists requested 0 (fun i ->
            [| if i > 0 then i - 1
               if i < requested - 1 then i + 1 |])
    | Grid3D ->
        let k = cubeSide requested
        fromLists (k * k * k) k (gridNeighbors k)
    | Imperfect3D ->
        let k = cubeSide requested
        let nodes = k * k * k
        let random = generator seed Stream.ExtraLinks 0

        fromLists nodes k (fun i ->
            let grid = gridNeighbors k i
            // One more neighbor, chosen uniformly among the nodes that are neither i nor one of its grid neighbors.
            // The link goes one way: i may send to it, but it is not given i as a neighbor in return.
            let mutable extra = random.Next nodes

            while extra = i || Array.contains extra grid do
                extra <- random.Next nodes

            Array.append grid [| extra |])

/// In a full network, the r-th other node (0 <= r < n - 1) as seen from node i: every node except i, in order.
let otherNode (i: int) (r: int) = if r >= i then r + 1 else r

/// A neighbor of node i. `pick bound` returns a number in [0, bound): the simulation passes a random number
/// generator, and --verify passes functions that return chosen numbers, to check every choice a node can make.
let inline chooseNeighbor (topology: Topology) (i: int) ([<InlineIfLambda>] pick: int -> int) =
    match topology.Shape with
    | Full -> otherNode i (pick (topology.Nodes - 1))
    | _ ->
        let first = topology.Offsets.[i]
        topology.Targets.[first + pick (topology.Offsets.[i + 1] - first)]

/// A neighbor of node i, chosen uniformly at random.
let randomNeighbor (topology: Topology) (i: int) (random: Rng) = chooseNeighbor topology i random.Next

/// --verify: checks the built topology. Returns what was checked, or the problems found.
let checkTopology (t: Topology) : Result<string, string list> =
    let n = t.Nodes
    let problems = ResizeArray<string>()

    let problem (text: string) =
        if problems.Count < 10 then
            problems.Add text

    /// The bound that node i passes to the random number generator when it picks a neighbor.
    let pickBound i =
        let bound = ref -1

        chooseNeighbor t i (fun b ->
            bound.Value <- b
            0)
        |> ignore

        bound.Value

    match t.Shape with
    | Full ->
        // Every node draws one of n - 1 numbers, and the lowest and the highest give other nodes. For the first,
        // middle and last node every number is checked: together they give each of the other nodes once.
        for i in 0 .. n - 1 do
            let bound = pickBound i

            if bound <> n - 1 then
                problem (sprintf "node %d picks one of %d numbers, expected %d" i bound (n - 1))
            else
                for r in [ 0; bound - 1 ] do
                    let j = chooseNeighbor t i (fun _ -> r)

                    if j < 0 || j >= n || j = i then
                        problem (sprintf "node %d: pick %d gives node %d" i r j)

        let fullyChecked = List.distinct [ 0; n / 2; n - 1 ]

        if problems.Count = 0 then
            for i in fullyChecked do
                let seen = Array.zeroCreate<bool> n

                for r in 0 .. n - 2 do
                    let j = chooseNeighbor t i (fun _ -> r)

                    if j < 0 || j >= n || j = i || seen.[j] then
                        problem (sprintf "node %d: pick %d gives node %d" i r j)
                    else
                        seen.[j] <- true

                let missed = Seq.init n id |> Seq.filter (fun j -> j <> i && not seen.[j]) |> Seq.length

                if missed > 0 then
                    problem (sprintf "node %d cannot pick %d of the other nodes" i missed)

        if problems.Count > 0 then
            Error(List.ofSeq problems)
        elif n = 2 then
            Ok "each node picks the other node (checked for nodes 0 and 1)"
        else
            Ok(
                sprintf
                    "every node picks among the other %d nodes (each pick checked for nodes 0, %d and %d)"
                    (n - 1)
                    (n / 2)
                    (n - 1)
            )
    | Line
    | Grid3D
    | Imperfect3D ->
        let k = t.Side
        let extra = if t.Shape = Imperfect3D then 1 else 0
        let neighbors i = t.Targets.[t.Offsets.[i] .. t.Offsets.[i + 1] - 1]
        let gridPart i = t.Targets.[t.Offsets.[i] .. t.Offsets.[i + 1] - 1 - extra]

        // Line: the two ends have 1 neighbor, the others 2. Grid: 6 minus the coordinates that are on the surface,
        // so corners have 3, edges 4, faces 5 and inner nodes 6.
        let expectedGridDegree i =
            match t.Shape with
            | Line -> if i = 0 || i = n - 1 then 1 else 2
            | _ ->
                let struct (x, y, z) = coordinates k i
                let onSurface c = if c = 0 || c = k - 1 then 1 else 0
                6 - onSurface x - onSurface y - onSurface z

        // Neighbors on the line differ by 1; neighbors in the grid differ by 1 in exactly one coordinate.
        let adjacent i j =
            match t.Shape with
            | Line -> abs (i - j) = 1
            | _ ->
                let struct (x1, y1, z1) = coordinates k i
                let struct (x2, y2, z2) = coordinates k j
                abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) = 1

        let nodesByDegree = Array.zeroCreate<int> 8 // no node has more than 7 neighbors

        for i in 0 .. n - 1 do
            let all = neighbors i
            nodesByDegree.[min all.Length 7] <- nodesByDegree.[min all.Length 7] + 1

            if all.Length <> expectedGridDegree i + extra then
                problem (sprintf "node %d has %d neighbors, expected %d" i all.Length (expectedGridDegree i + extra))

            if Array.exists (fun j -> j < 0 || j >= n) all then
                problem (sprintf "node %d has a neighbor outside 0 .. %d" i (n - 1))
            elif Array.contains i all then
                problem (sprintf "node %d is its own neighbor" i)
            elif (Array.distinct all).Length <> all.Length then
                problem (sprintf "node %d has a neighbor twice" i)
            else
                for j in gridPart i do
                    if not (adjacent i j) then
                        problem (sprintf "grid link %d -> %d does not join adjacent nodes" i j)
                    elif not (Array.contains i (gridPart j)) then
                        problem (sprintf "grid link %d -> %d has no link back" i j)

            // A random pick draws one of as many numbers as the node has neighbors, which give them in order.
            if all.Length > 0 then
                let bound = pickBound i

                if bound <> all.Length then
                    problem (sprintf "node %d picks one of %d numbers but has %d neighbors" i bound all.Length)
                else
                    for r in 0 .. bound - 1 do
                        if chooseNeighbor t i (fun _ -> r) <> all.[r] then
                            problem (sprintf "node %d: pick %d does not give neighbor %d" i r all.[r])

        // Connected: a breadth-first search from node 0 along the links reaches every node.
        let reached = Array.zeroCreate<bool> n
        let queue = Array.zeroCreate<int> n
        let mutable head = 0
        let mutable tail = 1
        reached.[0] <- true

        while head < tail do
            let i = queue.[head]
            head <- head + 1

            for p in t.Offsets.[i] .. t.Offsets.[i + 1] - 1 do
                let j = t.Targets.[p]

                if j >= 0 && j < n && not reached.[j] then
                    reached.[j] <- true
                    queue.[tail] <- j
                    tail <- tail + 1

        if tail < n then
            problem (sprintf "only %d of %d nodes can be reached from node 0" tail n)

        if problems.Count = 0 then
            let histogram =
                [ for d in 0..7 do
                      if nodesByDegree.[d] > 0 then
                          sprintf "%d with %d" nodesByDegree.[d] d ]
                |> String.concat ", "

            let links =
                if t.Shape = Imperfect3D then
                    "grid links join adjacent nodes and go both ways, extra links are new neighbors"
                else
                    "links join adjacent nodes and go both ways"

            [ sprintf "nodes by number of neighbors: %s" histogram
              "no self links or duplicates"
              links
              "connected"
              "every node picks among its neighbors" ]
            |> String.concat "; "
            |> Ok
        else
            Error(List.ofSeq problems)

// ---------------------------------------------------------------------------
// Messages

type Algorithm =
    | Gossip
    | PushSum

/// What every node needs to send: the topology and the actor of every node. Neither changes once sent.
type Network =
    { Topology: Topology
      Nodes: IActorRef[] }

/// What a gossip node reports when the run is over.
type GossipNodeReport =
    { Node: int
      Hearings: int
      Sends: int
      /// the round in which the node first heard the rumor (0 for the start node, -1 if never)
      FirstHeard: int
      /// the round in which it heard the rumor for the limit-th time (-1 if it has not)
      LimitReached: int
      /// Round messages that came when the node should not have been sending
      Misfires: int }

/// Bits of the change that a rumor caused at its receiver.
[<Literal>]
let Informed = 1

[<Literal>]
let Stopped = 2

type Message =
    // main program to the boss
    | Begin
    // boss to a node
    | Init of Network
    | StartRumor
    | Round of round: int
    | Collect of lastRound: int
    // between nodes
    | Rumor of round: int * sender: int
    | Pair of round: int * sender: int * s: float * w: float
    | Ack of round: int * receiver: int * change: int
    // node to the boss
    | Ready
    | GossipRoundDone of round: int * receiver: int * change: int
    | PushSumRoundDone of round: int * change: int
    | GossipReport of GossipNodeReport
    | PushSumReport of node: int * s: float * w: float * converged: bool

// ---------------------------------------------------------------------------
// Nodes

/// Gossip. The boss sends Round r only to the nodes that are active at the start of round r: they have heard the
/// rumor at least once and fewer than `limit` times. An active node sends the rumor to one random neighbor and,
/// once the neighbor has acknowledged it, tells the boss whether that neighbor has just heard the rumor for the
/// first time (Informed) or for the limit-th time (Stopped). Returns the node's message handler.
let gossipNode (seed: int) (limit: int) (index: int) (boss: IActorRef) =
    let random = generator seed Stream.NodeChoices index
    let mutable network = Unchecked.defaultof<Network>
    let mutable hearings = 0
    let mutable sends = 0
    let mutable firstHeard = -1
    let mutable limitReached = -1
    let mutable lastHeard = -1 // the last round in which the node heard the rumor
    let mutable heardInLastRound = 0 // and how often it heard it in that round
    let mutable misfires = 0

    let hear round =
        hearings <- hearings + 1

        if round <> lastHeard then
            lastHeard <- round
            heardInLastRound <- 0

        heardInLastRound <- heardInLastRound + 1
        let mutable change = 0

        if hearings = 1 then
            firstHeard <- round
            change <- change ||| Informed

        if hearings = limit then
            limitReached <- round
            change <- change ||| Stopped

        change

    fun (message: Message) ->
        match message with
        | Init net ->
            network <- net
            boss <! Ready
        | StartRumor -> hear 0 |> ignore // the start node hears the rumor in round 0
        | Round r ->
            // Rumors that arrive in round r count from round r + 1 on, even if they arrive before this message.
            let before = hearings - (if lastHeard = r then heardInLastRound else 0)

            if before < 1 || before >= limit then
                misfires <- misfires + 1

            sends <- sends + 1
            network.Nodes.[randomNeighbor network.Topology index random] <! Rumor(r, index)
        | Rumor(r, sender) -> network.Nodes.[sender] <! Ack(r, index, hear r)
        | Ack(r, receiver, change) -> boss <! GossipRoundDone(r, receiver, change)
        | Collect _ ->
            boss
            <! GossipReport
                { Node = index
                  Hearings = hearings
                  Sends = sends
                  FirstHeard = firstHeard
                  LimitReached = limitReached
                  Misfires = misfires }
        | _ -> ()

/// Pairs that arrived in a round: (sender, s, w).
type Inbox = ResizeArray<struct (int * float * float)>

let bySender =
    Comparison<struct (int * float * float)>(fun (struct (a, _, _)) (struct (b, _, _)) -> compare a b)

/// The termination rule of push-sum. Only rounds in which a node received at least one pair count. Such a round is
/// stable if s/w changed by at most RatioTolerance times its old value. A node counts as converged while its last
/// StableRounds counted rounds were all stable, and the run ends when every node counts as converged at once.
[<Literal>]
let RatioTolerance = 1e-10

[<Literal>]
let StableRounds = 3

/// Push-sum (Kempe, Dobra and Gehrke, FOCS 2003, Algorithm 1). Node i starts with s = i + 1 and w = 1. Round r
/// starts when the boss sends Round r, which also means that round r - 1 is over: the node first adds the pairs that
/// were sent to it in round r - 1 (in sender order) and applies the termination rule, then keeps half of s and w
/// and sends the other half to a random neighbor. It tells the boss when it starts or stops counting as converged.
/// Converged nodes go on exchanging, so no mass is lost. Returns the node's message handler.
let pushSumNode (seed: int) (index: int) (boss: IActorRef) =
    let random = generator seed Stream.NodeChoices index
    let mutable network = Unchecked.defaultof<Network>
    let mutable s = float (index + 1)
    let mutable w = 1.0
    let mutable stable = 0 // stable rounds with input, in a row
    let mutable reported = false // whether the boss counts this node as converged
    let mutable change = 0 // +1 or -1 if that has to change, to send with this round's PushSumRoundDone
    // Pairs by round parity: a pair for round r can arrive before this node has started round r, while the pairs of
    // round r - 1 have not been added yet. No pair of round r + 1 can be sent before this node has finished round r.
    let inbox = [| Inbox(); Inbox() |]

    /// Adds the pairs received in a round. `count` is false only for the last round, after the run has ended.
    let addReceived (round: int) (count: bool) =
        let pairs = inbox.[round &&& 1]

        if pairs.Count > 0 then
            if pairs.Count > 1 then
                pairs.Sort(bySender)

            let before = s / w

            for p in 0 .. pairs.Count - 1 do
                let struct (_, ps, pw) = pairs.[p]
                s <- s + ps
                w <- w + pw

            pairs.Clear()
            let after = s / w

            if count then
                if abs (after - before) <= RatioTolerance * abs before then
                    stable <- stable + 1
                else
                    stable <- 0

    fun (message: Message) ->
        match message with
        | Init net ->
            network <- net
            boss <! Ready
        | Round r ->
            addReceived (r - 1) true
            let converged = stable >= StableRounds
            change <- (if converged = reported then 0 elif converged then 1 else -1)
            reported <- converged
            s <- s * 0.5
            w <- w * 0.5
            network.Nodes.[randomNeighbor network.Topology index random] <! Pair(r, index, s, w)
        | Pair(r, sender, ps, pw) ->
            inbox.[r &&& 1].Add(struct (sender, ps, pw))
            network.Nodes.[sender] <! Ack(r, index, 0)
        | Ack(r, _, _) -> boss <! PushSumRoundDone(r, change)
        | Collect lastRound ->
            addReceived lastRound false
            boss <! PushSumReport(index, s, w, reported)
        | _ -> ()

/// A node actor. It passes every message to the node's handler, which `create` makes once, given the boss.
/// With a million nodes, this plain UntypedActor needed about 4 GB at the peak, where an Akka.FSharp actor { }
/// loop, which allocates new closures for every message, needed 6 to 9 GB; the rounds also took about 30% less time.
type NodeActor(create: IActorRef -> Message -> unit) =
    inherit UntypedActor()
    let handle = create UntypedActor.Context.Parent

    override this.OnReceive(message: obj) =
        match message with
        | :? Message as m -> handle m
        | _ -> this.Unhandled message

/// Creates node actors directly. Props.Create and Akka.FSharp's spawn build a LINQ expression for every actor,
/// which made starting a large network about four times slower and needed twice the memory.
type NodeProducer(create: IActorRef -> Message -> unit) =
    interface IIndirectActorProducer with
        member _.ActorType = typeof<NodeActor>
        member _.Produce() = new NodeActor(create) :> ActorBase
        member _.Release(_) = ()

// ---------------------------------------------------------------------------
// Boss: starts the nodes, runs the rounds and collects the results

type Settings =
    { Topology: Topology
      Algorithm: Algorithm
      Requested: int
      Seed: int
      Verify: bool
      MaxRounds: int
      RumorLimit: int
      Csv: bool
      BuildTime: TimeSpan }

type Status =
    | Converged
    | DiedOut // gossip: no node sends any more
    | CutOff // gossip: nodes still send, but none of them has a neighbor that has not heard the rumor
    | OutOfRounds

let shapeName shape =
    match shape with
    | Full -> "full"
    | Grid3D -> "3D"
    | Line -> "line"
    | Imperfect3D -> "imp3D"

let algorithmName algorithm =
    match algorithm with
    | Gossip -> "gossip"
    | PushSum -> "push-sum"

let statusName status =
    match status with
    | Converged -> "converged"
    | DiedOut -> "died-out"
    | CutOff -> "cut-off"
    | OutOfRounds -> "max-rounds"

let csvColumns =
    [ "topology"; "algorithm"; "nodes_requested"; "nodes"; "seed"; "rumor_limit"; "max_rounds"; "status"; "rounds"
      "messages"; "build_ms"; "start_ms"; "convergence_ms"; "informed"; "hearings_min"; "hearings_median"
      "hearings_max"; "converged_nodes"; "true_average"; "estimate_min"; "estimate_max"; "max_relative_error"
      "verify" ]

let csvHeader = String.concat "," csvColumns

/// A CSV row from (column, value) pairs. Columns that are not given are left empty.
let csvRow (values: (string * string) list) =
    for column, _ in values do
        if not (List.contains column csvColumns) then
            invalidArg (nameof values) (sprintf "no CSV column is called %s" column)

    csvColumns
    |> List.map (fun column ->
        match List.tryFind (fun (c, _) -> c = column) values with
        | Some(_, value) -> value
        | None -> "")
    |> String.concat ","

let invariant (x: float) = x.ToString("R", CultureInfo.InvariantCulture)

/// A small number in scientific notation, such as 3.1e-13.
let sci (x: float) =
    if x = 0.0 then "0" else x.ToString("0.0#e+0", CultureInfo.InvariantCulture)

let millis (t: TimeSpan) = t.TotalMilliseconds.ToString("0.0", CultureInfo.InvariantCulture)

/// The CSV columns that describe the run rather than its result.
let runColumns (settings: Settings) =
    [ "topology", shapeName settings.Topology.Shape
      "algorithm", algorithmName settings.Algorithm
      "nodes_requested", string settings.Requested
      "nodes", string settings.Topology.Nodes
      "seed", string settings.Seed
      "rumor_limit", (if settings.Algorithm = Gossip then string settings.RumorLimit else "")
      "max_rounds", string settings.MaxRounds
      "build_ms", millis settings.BuildTime ]

/// What the boss hands to the main program when a run is over: the exit code, and the report or the CSV row.
type Outcome = { ExitCode: int; Lines: string list }

/// The start node of gossip.
let startNode (seed: int) (nodes: int) = (generator seed Stream.StartNode 0).Next nodes

/// For each node, the nodes that can send to it: the neighbor lists turned around. They are the same lists except for
/// the extra imp3D links, which go one way. Node j's senders are Sources.[Offsets.[j] .. Offsets.[j + 1] - 1].
let senderLists (t: Topology) =
    let offsets = Array.zeroCreate (t.Nodes + 1)

    for j in t.Targets do
        offsets.[j + 1] <- offsets.[j + 1] + 1

    for j in 0 .. t.Nodes - 1 do
        offsets.[j + 1] <- offsets.[j + 1] + offsets.[j]

    let next = Array.copy offsets
    let sources = Array.zeroCreate t.Targets.Length

    for v in 0 .. t.Nodes - 1 do
        for p in t.Offsets.[v] .. t.Offsets.[v + 1] - 1 do
            let j = t.Targets.[p]
            sources.[next.[j]] <- v
            next.[j] <- next.[j] + 1

    offsets, sources

/// The boss. It completes `finished` with the outcome of the run, or with the exception if one of its message
/// handlers throws.
let BossActor (settings: Settings) (finished: TaskCompletionSource<Outcome>) (mailbox: Actor<Message>) =
    let topology = settings.Topology
    let n = topology.Nodes
    let nodes: IActorRef[] = Array.zeroCreate n
    let startClock = Stopwatch()
    let roundClock = Stopwatch()
    let mutable ready = 0
    let mutable round = 0
    let mutable pending = 0 // RoundDone messages still expected in this round
    let mutable messages = 0L // rumors or pairs sent between nodes
    let mutable status = Converged
    let mutable reports = 0
    let say (text: string) = if not settings.Csv then printfn "%s" text

    /// A node reports the end of a round only for the round in progress; anything else is a bug in the rounds.
    let checkRound (reported: int) =
        if reported <> round then
            failwithf "a node reported the end of round %d during round %d" reported round

    // gossip: the nodes that send in this round, the nodes that heard the rumor for the first time in this round,
    // and the nodes that have heard it `limit` times
    let mutable active: int[] = [||]
    let newlyInformed = ResizeArray<int>()
    let stopped: bool[] = if settings.Algorithm = Gossip then Array.zeroCreate n else [||]
    let mutable informed = 0
    let gossipReports: GossipNodeReport[] = if settings.Algorithm = Gossip then Array.zeroCreate n else [||]

    // gossip on a topology with neighbor lists: for each node, how many of its neighbors have not heard the rumor.
    // A node that never hears the rumor `limit` times sends until the run ends, so the run cannot wait for every
    // node to stop. Instead it stops when no node that still sends has a neighbor left to tell.
    let hasLists = settings.Algorithm = Gossip && topology.Shape <> Full
    let senderOffsets, senders = if hasLists then senderLists topology else ([||], [||])
    let unheardNeighbors: int[] = if hasLists then Array.init n topology.Degree else [||]

    /// Node u has heard the rumor for the first time.
    let firstHeard (u: int) =
        informed <- informed + 1

        if hasLists then
            for p in senderOffsets.[u] .. senderOffsets.[u + 1] - 1 do
                let v = senders.[p]
                unheardNeighbors.[v] <- unheardNeighbors.[v] - 1

    // push-sum: the number of nodes that count as converged, and the final values
    let mutable converged = 0
    let finalS: float[] = if settings.Algorithm = PushSum then Array.zeroCreate n else [||]
    let finalW: float[] = if settings.Algorithm = PushSum then Array.zeroCreate n else [||]
    let mutable reportedConverged = 0

    let finish result =
        roundClock.Stop()
        status <- result
        let collect = Collect round

        for node in nodes do
            node <! collect

    let startGossipRound () =
        round <- round + 1
        pending <- active.Length
        messages <- messages + int64 active.Length
        let message = Round round

        for i in active do
            nodes.[i] <! message

    let endGossipRound () =
        if informed = n then
            finish Converged
        else
            let next = ResizeArray<int>(active.Length + newlyInformed.Count)

            for i in active do
                if not stopped.[i] then
                    next.Add i

            for i in newlyInformed do
                if not stopped.[i] then
                    next.Add i

            newlyInformed.Clear()
            active <- next.ToArray()

            if active.Length = 0 then
                finish DiedOut
            elif hasLists && not (Array.exists (fun v -> unheardNeighbors.[v] > 0) active) then
                finish CutOff
            elif round >= settings.MaxRounds then
                finish OutOfRounds
            else
                startGossipRound ()

    let startPushSumRound () =
        round <- round + 1
        pending <- n
        messages <- messages + int64 n
        let message = Round round

        for node in nodes do
            node <! message

    let endPushSumRound () =
        if converged = n then finish Converged
        elif round >= settings.MaxRounds then finish OutOfRounds
        else startPushSumRound ()

    let startNodes () =
        startClock.Start()

        for i in 0 .. n - 1 do
            let create =
                match settings.Algorithm with
                | Gossip -> gossipNode settings.Seed settings.RumorLimit i
                | PushSum -> pushSumNode settings.Seed i

            nodes.[i] <- mailbox.Context.ActorOf(Props.CreateBy(NodeProducer create), "node" + string i)

        let init = Init({ Topology = topology; Nodes = nodes }: Network)

        for node in nodes do
            node <! init

    let run () =
        startClock.Stop()
        say (sprintf "%d actors started in %s ms" n (millis startClock.Elapsed))

        match settings.Algorithm with
        | Gossip ->
            let start = startNode settings.Seed n
            nodes.[start] <! StartRumor
            firstHeard start

            if settings.RumorLimit > 1 then
                active <- [| start |]
            else
                stopped.[start] <- true

            roundClock.Start()

            if active.Length = 0 then finish DiedOut else startGossipRound ()
        | PushSum ->
            roundClock.Start()
            startPushSumRound ()

    /// The results of a gossip run, and the --verify checks.
    let gossipSummary () =
        let hearings = gossipReports |> Array.map (fun r -> r.Hearings)
        let heard = hearings |> Array.filter (fun h -> h > 0) |> Array.length
        let sorted = Array.sort hearings

        let median =
            if n % 2 = 1 then
                float sorted.[n / 2]
            else
                float (sorted.[n / 2 - 1] + sorted.[n / 2]) / 2.0

        let totalHearings = hearings |> Array.sumBy int64

        let checks =
            if not settings.Verify then
                None
            else
                // Active in rounds FirstHeard + 1 to LimitReached (or to the last round): one send in each.
                let expectedSends (r: GossipNodeReport) =
                    if r.FirstHeard < 0 then 0
                    elif r.LimitReached >= 0 then r.LimitReached - r.FirstHeard
                    else round - r.FirstHeard

                let wrongSends =
                    gossipReports |> Array.filter (fun r -> r.Sends <> expectedSends r || r.Misfires > 0)

                let problems =
                    [ if heard < n then
                          sprintf "%d of %d nodes never heard the rumor" (n - heard) n
                      if totalHearings <> messages + 1L then
                          sprintf
                              "the nodes heard the rumor %d times, expected messages sent + 1 = %d"
                              totalHearings
                              (messages + 1L)
                      if informed <> heard then
                          sprintf "the boss counted %d informed nodes, the nodes report %d" informed heard
                      if wrongSends.Length > 0 then
                          let r = wrongSends.[0]

                          sprintf
                              "%d nodes did not send once per round while active, e.g. node %d sent %d times, expected %d"
                              wrongSends.Length
                              r.Node
                              r.Sends
                              (expectedSends r) ]

                if List.isEmpty problems then
                    let whileActive =
                        if settings.RumorLimit = 2 then
                            "once"
                        else
                            sprintf "1 to %d times" (settings.RumorLimit - 1)

                    [ sprintf "all %d nodes heard the rumor" n
                      sprintf "messages sent = hearings - 1 (the start node's): %d = %d - 1" messages totalHearings
                      sprintf "every node sent once in each round that it started having heard the rumor %s" whileActive
                      + ", and in no other round" ]
                    |> String.concat "; "
                    |> Ok
                    |> Some
                else
                    Some(Error problems)

        let lines =
            [ sprintf "Coverage: %d of %d nodes heard the rumor" heard n
              sprintf "Hearings per node: min %d, median %g, max %d" sorted.[0] median sorted.[n - 1] ]

        let csv =
            [ "informed", string heard
              "hearings_min", string sorted.[0]
              "hearings_median", sprintf "%g" median
              "hearings_max", string sorted.[n - 1] ]

        lines, csv, checks

    /// The results of a push-sum run, and the --verify checks.
    let pushSumSummary () =
        let average = float (int64 n + 1L) / 2.0
        let estimates = Array.init n (fun i -> finalS.[i] / finalW.[i])
        let lowest = Array.min estimates
        let highest = Array.max estimates
        let maxError = estimates |> Array.map (fun e -> abs (e - average) / average) |> Array.max
        // Sums in node order, so that they are the same in every run with the same seed
        let mutable totalS = 0.0
        let mutable totalW = 0.0

        for i in 0 .. n - 1 do
            totalS <- totalS + finalS.[i]
            totalW <- totalW + finalW.[i]

        let expectedS = float n * float (int64 n + 1L) / 2.0
        let errorS = abs (totalS - expectedS) / expectedS
        let errorW = abs (totalW - float n) / float n

        let checks =
            if not settings.Verify then
                None
            else
                let problems =
                    [ if reportedConverged < n then
                          sprintf "%d of %d nodes have not converged" (n - reportedConverged) n
                      if errorW > 1e-9 then
                          sprintf "total w is %s, expected %d (relative error %s)" (invariant totalW) n (sci errorW)
                      if errorS > 1e-9 then
                          sprintf
                              "total s is %s, expected %s (relative error %s)"
                              (invariant totalS)
                              (invariant expectedS)
                              (sci errorS)
                      if maxError > 1e-6 then
                          sprintf "max relative error of the estimates is %s, more than 1e-6" (sci maxError) ]

                if List.isEmpty problems then
                    [ sprintf "all %d nodes converged" n
                      sprintf "total w = %s (relative error %s)" (invariant totalW) (sci errorW)
                      sprintf "total s = %s (relative error %s)" (invariant totalS) (sci errorS)
                      sprintf "max relative error %s <= 1e-6" (sci maxError) ]
                    |> String.concat "; "
                    |> Ok
                    |> Some
                else
                    Some(Error problems)

        let lines =
            [ sprintf "Converged nodes: %d of %d" reportedConverged n
              sprintf "True average (N + 1) / 2: %s" (invariant average)
              sprintf
                  "Estimates: min %s, max %s, max relative error %s"
                  (invariant lowest)
                  (invariant highest)
                  (sci maxError) ]

        let csv =
            [ "converged_nodes", string reportedConverged
              "true_average", invariant average
              "estimate_min", invariant lowest
              "estimate_max", invariant highest
              "max_relative_error", invariant maxError ]

        lines, csv, checks

    let summarize () =
        let lines, csvPart, checks =
            match settings.Algorithm with
            | Gossip -> gossipSummary ()
            | PushSum -> pushSumSummary ()

        let verified =
            match checks with
            | None -> ""
            | Some(Ok _) -> "passed"
            | Some(Error _) -> "failed"

        let report =
            if settings.Csv then
                [ csvRow (
                      runColumns settings
                      @ [ "status", statusName status
                          "rounds", string round
                          "messages", string messages
                          "start_ms", millis startClock.Elapsed
                          "convergence_ms", millis roundClock.Elapsed ]
                      @ csvPart
                      @ [ ("verify", verified) ]
                  ) ]
            else
                [ match status with
                  | Converged -> ()
                  | DiedOut ->
                      sprintf
                          "The rumor died out after round %d: every node that heard it has heard it %s"
                          round
                          (if settings.RumorLimit = 1 then "once" else sprintf "%d times" settings.RumorLimit)
                  | CutOff ->
                      sprintf
                          "The rumor stopped spreading after round %d: the %d nodes still sending have no neighbor left to tell"
                          round
                          active.Length
                  | OutOfRounds -> sprintf "Not converged after %d rounds (--max-rounds %d)" round settings.MaxRounds

                  if status = Converged then
                      sprintf "Convergence time: %s ms" (millis roundClock.Elapsed)
                  else
                      sprintf "Time: %s ms, without converging" (millis roundClock.Elapsed)

                  sprintf "Rounds: %d" round
                  sprintf "Messages sent: %d" messages

                  for line in lines do
                      line

                  match checks with
                  | None -> ()
                  | Some(Ok text) -> sprintf "Verify %s: OK: %s" (algorithmName settings.Algorithm) text
                  | Some(Error problems) ->
                      sprintf "Verify %s: FAILED" (algorithmName settings.Algorithm)

                      for p in problems do
                          sprintf "  %s" p ]

        let failed =
            match checks with
            | Some(Error _) -> true
            | _ -> false

        finished.TrySetResult
            { ExitCode = (if status <> Converged || failed then 1 else 0)
              Lines = report }
        |> ignore

    let handle message =
        match message with
        | Begin -> startNodes ()
        | Ready ->
            ready <- ready + 1

            if ready = n then
                run ()
        | GossipRoundDone(r, receiver, change) ->
            checkRound r

            if (change &&& Informed) <> 0 then
                firstHeard receiver
                newlyInformed.Add receiver

            if (change &&& Stopped) <> 0 then
                stopped.[receiver] <- true

            pending <- pending - 1

            if pending = 0 then
                endGossipRound ()
        | PushSumRoundDone(r, change) ->
            checkRound r
            converged <- converged + change
            pending <- pending - 1

            if pending = 0 then
                endPushSumRound ()
        | GossipReport report ->
            gossipReports.[report.Node] <- report
            reports <- reports + 1

            if reports = n then
                summarize ()
        | PushSumReport(i, s, w, nodeConverged) ->
            finalS.[i] <- s
            finalW.[i] <- w

            if nodeConverged then
                reportedConverged <- reportedConverged + 1

            reports <- reports + 1

            if reports = n then
                summarize ()
        | _ -> ()

    // An exception ends the run (for example, running out of memory while starting the nodes). Restarting the boss
    // would lose the state of the run and leave the main program waiting.
    let handleOrStop message =
        try
            handle message
        with e ->
            finished.TrySetException e |> ignore

    let rec loop () =
        actor {
            let! message = mailbox.Receive()
            handleOrStop message
            return! loop ()
        }

    loop ()

// ---------------------------------------------------------------------------
// Main

/// The default limit on rounds, which grows with the network. Full networks and imp3D get 1000 + 50 log2 n for both
/// algorithms. Gossip on a full network needs that room: a few runs take several times the usual number of rounds,
/// because the last nodes that have not heard the rumor can only be told by the few nodes that still send it.
/// Grids and lines get a multiple of their diameter D (n - 1 for a line, 3(k - 1) for a k x k x k grid), or of D²
/// for push-sum, which spreads its values by diffusion there.
let defaultMaxRounds (algorithm: Algorithm) (topology: Topology) =
    let n = topology.Nodes

    match topology.Shape with
    | Full
    | Imperfect3D -> 1000 + 50 * int (ceil (Math.Log2(float n)))
    | Grid3D
    | Line ->
        let diameter = if topology.Shape = Line then n - 1 else 3 * (topology.Side - 1)

        match algorithm with
        | Gossip -> 100 + 10 * diameter
        | PushSum -> int (min 1_000_000_000L (1000L + 50L * int64 diameter * int64 diameter))

let usage =
    String.concat
        "\n"
        [ "usage: dotnet fsi GossipSimulator.fsx <numNodes> <topology> <algorithm> [--seed <n>] [--verify]"
          "                                      [--max-rounds <n>] [--rumor-limit <n>] [--csv]"
          "       dotnet fsi GossipSimulator.fsx --csv-header"
          "  numNodes   2 to 100000000 (3D and imp3D round it up to the next perfect cube)"
          "  topology   full, 3D, line or imp3D"
          "  algorithm  gossip or push-sum"
          "  --seed     -2147483648 to 2147483647" ]

type Options =
    { Nodes: int
      Shape: Shape
      Algorithm: Algorithm
      Seed: int option
      Verify: bool
      MaxRounds: int option
      RumorLimit: int
      Csv: bool }

type Command =
    | Run of Options
    | PrintCsvHeader

/// A whole number from the command line between `low` and `high`, or an error that says what is wrong with it.
let parseNumber (name: string) (low: int) (high: int) (text: string) =
    match BigInteger.TryParse(text, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
    | true, value when value >= BigInteger low && value <= BigInteger high -> Ok(int value)
    | true, _ -> Error(sprintf "%s must be between %d and %d: %s" name low high text)
    | false, _ -> Error(sprintf "%s must be a whole number: %s" name text)

let rec parseOptions (options: Options) (args: string list) =
    match args with
    | [] -> Ok options
    | "--verify" :: rest -> parseOptions { options with Verify = true } rest
    | "--csv" :: rest -> parseOptions { options with Csv = true } rest
    | "--seed" :: value :: rest ->
        parseNumber "--seed" Int32.MinValue Int32.MaxValue value
        |> Result.bind (fun seed -> parseOptions { options with Seed = Some seed } rest)
    | "--max-rounds" :: value :: rest ->
        parseNumber "--max-rounds" 1 Int32.MaxValue value
        |> Result.bind (fun rounds -> parseOptions { options with MaxRounds = Some rounds } rest)
    | "--rumor-limit" :: value :: rest ->
        parseNumber "--rumor-limit" 1 Int32.MaxValue value
        |> Result.bind (fun limit -> parseOptions { options with RumorLimit = limit } rest)
    | [ ("--seed" | "--max-rounds" | "--rumor-limit") as option ] -> Error(sprintf "%s needs a number" option)
    | other :: _ -> Error(sprintf "unknown option: %s" other)

let parseArguments (args: string list) =
    match args with
    | [ "--csv-header" ] -> Ok PrintCsvHeader
    | nodes :: shape :: algorithm :: options ->
        let shape =
            match shape.ToLowerInvariant() with
            | "full" -> Ok Full
            | "3d" -> Ok Grid3D
            | "line" -> Ok Line
            | "imp3d" -> Ok Imperfect3D
            | _ -> Error(sprintf "unknown topology: %s" shape)

        let algorithm =
            match algorithm.ToLowerInvariant() with
            | "gossip" -> Ok Gossip
            | "push-sum" -> Ok PushSum
            | _ -> Error(sprintf "unknown algorithm: %s" algorithm)

        match parseNumber "numNodes" 2 100_000_000 nodes, shape, algorithm with
        | Error e, _, _
        | _, Error e, _
        | _, _, Error e -> Error e
        | Ok n, Ok shape, Ok algorithm ->
            parseOptions
                ({ Nodes = n
                   Shape = shape
                   Algorithm = algorithm
                   Seed = None
                   Verify = false
                   MaxRounds = None
                   RumorLimit = 10
                   Csv = false }: Options)
                options
            |> Result.map Run
    | _ -> Error "expected <numNodes> <topology> <algorithm>"

let describe (options: Options) (topology: Topology) =
    let k = topology.Side

    let network =
        match topology.Shape with
        | Full -> "a full network"
        | Line -> "a line"
        | Grid3D -> sprintf "a 3D grid (%d x %d x %d)" k k k
        | Imperfect3D -> sprintf "an imperfect 3D grid (%d x %d x %d, plus one random neighbor per node)" k k k

    let rounded =
        if topology.Nodes <> options.Nodes then
            sprintf " (%d rounded up to a cube)" options.Nodes
        else
            ""

    let name =
        match options.Algorithm with
        | Gossip -> "Gossip"
        | PushSum -> "Push-sum"

    sprintf "%s on %s: %d nodes%s" name network topology.Nodes rounded

/// Runs the simulation in an actor system and returns the exit code. The report, or the CSV row, goes to `output`.
let simulate (settings: Settings) (output: TextWriter) =
    let config =
        Akka.Configuration.ConfigurationFactory.ParseString
            """
            akka {
                loglevel = WARNING
                stdout-loglevel = WARNING
                log-dead-letters = off
                log-dead-letters-during-shutdown = off
                # Stopping a million node actors can take about 10 s, the default limit of this phase.
                coordinated-shutdown.phases.actor-system-terminate.timeout = 300 s
                # The program stops the actor system itself. After an error it waits only 30 s for that, and
                # leaving the process should not wait again.
                coordinated-shutdown.run-by-clr-shutdown-hook = off
            }
            """

    let system = ActorSystem.Create("gossip", config)
    let finished = TaskCompletionSource<Outcome>()

    // A node that throws ends the run: the boss stops it and hands the exception to the main program.
    let nodeFailed =
        OneForOneStrategy(
            (fun (e: exn) ->
                finished.TrySetException e |> ignore
                Directive.Stop),
            false
        )

    let boss =
        spawnOpt system "boss" (BossActor settings finished) [ SpawnOption.SupervisorStrategy nodeFailed ]

    // The boss itself stops only if it cannot be created, for example for lack of memory.
    let bossStopped = boss.WatchAsync()
    boss <! Begin
    Task.WaitAny(finished.Task, bossStopped) |> ignore

    if finished.Task.IsCompletedSuccessfully then
        for line in finished.Task.Result.Lines do
            output.WriteLine line

        system.Terminate().Wait()
        finished.Task.Result.ExitCode
    else
        let error =
            if finished.Task.IsFaulted then
                string finished.Task.Exception.InnerException
            else
                "the boss actor stopped"

        eprintfn "Error: the run stopped: %s" error

        if settings.Csv then
            output.WriteLine(csvRow (runColumns settings @ [ ("status", "error") ]))

        // After running out of memory, stopping the actors that were started can take much longer than usual.
        system.Terminate().Wait(TimeSpan.FromSeconds 30.0) |> ignore
        1

match parseArguments (List.tail (List.ofArray fsi.CommandLineArgs)) with
| Error message ->
    eprintfn "%s" message
    eprintfn "%s" usage
    exit 2
| Ok PrintCsvHeader ->
    printfn "%s" csvHeader
    exit 0
| Ok(Run options) ->
    // With --csv, stdout carries only the CSV row: everything else, including Akka's log, goes to stderr.
    let output = Console.Out

    if options.Csv then
        Console.SetOut Console.Error

    let seed = defaultArg options.Seed (Random.Shared.Next())
    let say (text: string) = if not options.Csv then printfn "%s" text
    let buildClock = Stopwatch.StartNew()
    let topology = buildTopology options.Shape options.Nodes seed
    buildClock.Stop()
    let maxRounds = defaultArg options.MaxRounds (defaultMaxRounds options.Algorithm topology)

    let settings: Settings =
        { Topology = topology
          Algorithm = options.Algorithm
          Requested = options.Nodes
          Seed = seed
          Verify = options.Verify
          MaxRounds = maxRounds
          RumorLimit = options.RumorLimit
          Csv = options.Csv
          BuildTime = buildClock.Elapsed }

    let seedText =
        if options.Seed.IsSome then
            sprintf "seed %d" seed
        else
            sprintf "seed %d (random; repeat the run with --seed %d)" seed seed

    say (sprintf "%s, %s" (describe options topology) seedText)

    match options.Algorithm with
    | Gossip ->
        say (
            sprintf
                "Start node %d, rumor limit %d, at most %d rounds"
                (startNode seed topology.Nodes)
                options.RumorLimit
                maxRounds
        )
    | PushSum -> say (sprintf "At most %d rounds" maxRounds)

    let links =
        if topology.Shape = Full then
            ""
        else
            sprintf " (%d links)" topology.Targets.Length

    say (sprintf "Topology built in %s ms%s" (millis buildClock.Elapsed) links)

    let topologyCheck =
        if options.Verify then
            let result = checkTopology topology

            match result with
            | Ok text -> say (sprintf "Verify topology: OK: %s" text)
            | Error problems ->
                // also with --csv, on stderr
                printfn "Verify topology: FAILED"

                for p in problems do
                    printfn "  %s" p

            Some result
        else
            None

    let code =
        match topologyCheck with
        | Some(Error _) ->
            if options.Csv then
                output.WriteLine(csvRow (runColumns settings @ [ "status", "topology-failed"; "verify", "failed" ]))

            1
        | _ -> simulate settings output

    exit code
