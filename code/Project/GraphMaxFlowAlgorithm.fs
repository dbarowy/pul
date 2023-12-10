module GraphMaxFlowAlgorithm

open Graph
open AST

open System

let BFS (g: int * Vertex<'a,int> list)(souceID: int) (sinkID: int) (parent: int array): bool =
    printfn "BFS CALLED"
    let vertexCount: int = (g |> snd) |> List.length
    //printfn "%A" vertexCount

    let mutable visited: bool array = Array.init  (g |> fst) (fun i -> if i = souceID then true else false)
    //printfn "%A" visited

    let mutable queue: int list = [0]

    let mutable result: bool = false

    while (List.length queue > 0 && (not result)) do 
        //printfn "Queue: %A" queue

        // Pop off first element of queue
        let popID: int = List.head queue
        queue <- (List.tail queue)

        let popVertex: Vertex<'a,int>= getVertex popID g
        let popVertexAdjacencyList: Adjacency<int>= popVertex |> snd
        let popAdjacentVerticiesIDs: (int * int) list = List.map (fun (_,_,v,cap) -> (v,cap)) popVertexAdjacencyList
        //printfn "%A" popAdjacentVerticiesIDs

        let rec work (vertexList: (int * int) list): bool =  
            //printfn "VextexList: %A" vertexList
            match vertexList with
            | [] -> 
                //printfn "MADE IT TO FALSE"
                false
            | x::xs -> 
                let index: int  = x |> fst
                let value: int = x |> snd
                //printfn "Index: %A" index
                //printfn "Value: %A" value
                //printfn "sinkID: %A" sinkID
                //printfn "1st bool: %A" (visited.[index] = false)
                //printfn "2nd bool: %A" (value > 0)
                //printfn "3rd bool: %A" (index = sinkID)
                if (visited.[index] = false) && (value > 0)  && (index = sinkID)  then
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    //printfn "HIT TRUE"
                    true
                elif (visited.[index] = false) && (value > 0) then
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    //printfn "ENTERED ELIF"
                    work xs
                else 
                    //printfn "ENTERED ELSE"
                    work xs

        result <- work popAdjacentVerticiesIDs 
    
    //printfn "PARENT: %A" parent
    result

let findEdgeIDAndCap (u: int) (v: int) (g: Graph<'a,int>) =
    let allEdges: Adjacency<'b> = getEdges u g 
    let rec findMatch (edgeList: (int * 'c * int * int) list): int * int =
        match edgeList with
        | []    -> (-1,-1)
        | (id,_,nextNode,cap)::xs ->
            if nextNode = v then (id,cap) else findMatch xs
    findMatch allEdges


let fordFulkerson (g: Graph<'a,'b>)(sourceID: int) (sinkID: int) =
    let mutable mutableG: Graph<'a,int> = g

    let parent: int array = Array.init (g |> fst) (fun i -> -1)

    let mutable max_flow: int = 0

    let mutable pathExists = BFS mutableG sourceID sinkID parent
    //printfn "PATH_EXISTS: %A" pathExists

    while pathExists do
        let mutable path_flow: int = 2147483647
        let mutable s: int = sinkID
        while (s <> sourceID) do
            let parentID: int = parent.[s]
            let parentEdges: Adjacency<int> = getEdges parentID mutableG
            let parentToSFlow: int = List.fold (fun acc (_,_,id, cap) -> if id = s then acc+cap else acc ) 0 parentEdges
            path_flow <- (min path_flow parentToSFlow)
            s <- parentID
        
        max_flow <- max_flow + path_flow

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

            v <- parent.[v]
        
        pathExists <- BFS mutableG sourceID sinkID parent

    max_flow


