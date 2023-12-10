module GraphMaxFlowAlgorithm

open Graph
open AST

open System

let BFS (g: Graph<'a,'b>)(souceID: int) (sinkID: int) (parent: int array): bool =
    let vertexCount: int = (g |> snd) |> List.length
    //printfn "%A" vertexCount

    let mutable visited: bool array = Array.init  (g |> fst) (fun i -> if i = 0 then true else false)
    //printfn "%A" visited

    let mutable queue: int list = [0]

    let mutable result: bool = false

    while (List.length queue > 0 && (not result)) do 
        //printfn "Queue: %A" queue

        // Pop off first element of queue
        let popID: int = List.head queue
        queue <- (List.tail queue)

        let popVertex: Vertex<'a,'b> = getVertex popID g
        let popVertexAdjacencyList: EdgeData<'E> list = popVertex |> snd
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
                //printfn "Value: %A" index
                if (visited.[index] = false) && (value > 0)  && (index = sinkID)  then
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    true
                elif (visited.[index] = false) && (value > 0)then
                    queue <- (List.append queue [index])
                    visited.[index] <- true
                    parent.[index] <- popID
                    work xs
                else 
                    //printfn "ENTERED ELSE"
                    work xs

        result <- work popAdjacentVerticiesIDs 
    
    //printfn "PARENT: %A" parent
    result

(*let fordFulkerson (g: Graph<'a,'b>)(sourceID: int) (sinkID: int) =
    let mutable mutableG: Graph<'a,(int * int)> = g

    let parent: int array = Array.init (g |> fst) (fun i -> -1)

    let mutable max_flow: int = 0

    let mutable pathExists = BFS mutableG sourceID sinkID parent

    while pathExists do
        let mutable path_flow: int = 2147483647
        let mutable s: int = sinkID
        while (s <> sourceID) do
            let parentID: int = parent.[s]
            let parentEdges: Adjacency<int * int> = getEdges parentID mutableG
            let parentToSFlow: int = List.fold (fun acc (_,_,id,(flow, cap)) -> if id = s then acc+cap-flow else acc ) 0 parentEdges
            path_flow <- (min path_flow parentToSFlow)
            s <- parentID
        
        max_flow <- max_flow + path_flow


    0*)


