module GraphMaxFlowAlgorithm

open Graph

let BFS (g: int * Vertex<'a,int> list)(souceID: int) (sinkID: int) (parent: int array): bool =
    // Save how many vertices there are in the graph
    let vertexCount: int = (g |> snd) |> List.length

    // Initialize a visited array with just the source visited
    let mutable visited: bool array = Array.init  (g |> fst) (fun i -> if i = souceID then true else false)

    // Initialize a queue with just the source ID
    let mutable queue: int list = [0]

    // Initialize a result boolean
    let mutable result: bool = false

    // While there are still nodes to explore and we haven't found the sink yet
    while (List.length queue > 0 && (not result)) do 
        // Pop off first element of queue
        let popID: int = List.head queue
        queue <- (List.tail queue)

        // Get the vertex and the verticies it's adjacent to
        let popVertex: Vertex<'a,int>= getVertex popID g
        let popVertexAdjacencyList: Adjacency<int>= popVertex |> snd
        let popAdjacentVerticiesIDs: (int * int) list = List.map (fun (_,_,v,cap) -> (v,cap)) popVertexAdjacencyList

        // While the popped vertex has neighbors
        let rec work (vertexList: (int * int) list): bool =  
            // Matched the neighbors list with
            match vertexList with
            // No more neighbors to explore
            | []    -> false
            // At least one neighbor to explore
            | x::xs -> 
                let index: int  = x |> fst
                let value: int = x |> snd
                // Is this an 
                //     1) unexplored vertx?
                //     2) connected by an edge that has space for more flow?
                //     3) the sink?
                if (visited.[index] = false) && (value > 0)  && (index = sinkID)  then
                    // Yes, then we have found a path to the sink and are done
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    true
                // Is this an 
                //     1) unexplored vertx?
                //     2) connected by an edge that has space for more flow?
                elif (visited.[index] = false) && (value > 0) then
                    // Yes, then we need to append it to the queue, update visited, and add its parent node to the path
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    work xs
                // Otherwise move onto the next node
                else 
                    work xs

        result <- work popAdjacentVerticiesIDs 

    result

// Find the edge ID from u to v and its capacity
let findEdgeIDAndCap (u: int) (v: int) (g: Graph<'a,int>) =
    let allEdges: Adjacency<int>= getEdges u g 
    let rec findMatch (edgeList: (int * 'c * int * int) list): int * int =
        match edgeList with
        | []    -> (-1,-1)
        | (id,_,nextNode,cap)::xs ->
            if nextNode = v then (id,cap) else findMatch xs
    findMatch allEdges


let fordFulkerson (g: Graph<'a,int>)(sourceID: int) (sinkID: int) =
    // Make the graph mutable
    let mutable mutableG: Graph<'a,int>= g

    // Initialize a parent array to keep track of the last path to the sink
    let parent: int array = Array.init (g |> fst) (fun i -> -1)

    // Initializa a mutable max_flow we'll add to
    let mutable max_flow: int = 0

    // Initialize a mutable path exists boolean that is true when BFS says we can get to the sink
    let mutable pathExists: bool = BFS mutableG sourceID sinkID parent

    // While there is a path to the sink
    while pathExists do
        // Initialize a dummy path_flow set to max int
        let mutable path_flow: int = 2147483647
        // Starting at the sink
        let mutable s: int = sinkID
        // While we are not at the source
        while (s <> sourceID) do
            // Find the parent, calculate the new min flow of the path, move to the parent
            let parentID: int = parent.[s]
            let parentEdges: Adjacency<int> = getEdges parentID mutableG
            let parentToSFlow: int = List.fold (fun acc (_,_,id, cap) -> if id = s then acc+cap else acc ) 0 parentEdges
            path_flow <- (min path_flow parentToSFlow)
            s <- parentID
        
        // Add this path_flow to max_flow
        max_flow <- max_flow + path_flow

        // Starting at the sink
        let mutable v: int = sinkID
        while (v <> sourceID) do
            let u: int = parent.[v]

            // Subtract flow from u -> v edge (forward)
            let forwardEdgeData: int * int = findEdgeIDAndCap u v mutableG
            let ID: int = forwardEdgeData |> fst
            let Cap: int = forwardEdgeData |> snd
            mutableG <- (removeEdge ID mutableG)
            mutableG <- ((addEdge 0 u v (Cap-path_flow) mutableG) |> snd)

            // Add flow from v -> edge (backward)
            let backwardEdgeData: int * int = findEdgeIDAndCap v u mutableG
            let ID: int = backwardEdgeData |> fst
            let Cap: int = backwardEdgeData |> snd
            mutableG <- (removeEdge ID mutableG)
            mutableG <- ((addEdge 0 v u (Cap+path_flow) mutableG) |> snd)

            // Move to the parent
            v <- parent.[v]
        
        pathExists <- BFS mutableG sourceID sinkID parent

    (max_flow,mutableG)


