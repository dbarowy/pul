module GraphMaxFlowAlgorithm

open Graph
open AST

open System

let BFS (g: Graph<'a,'b>)(souceID: int) (sinkID: int) (parent: int array): bool =
    let vertexCount: int = (g |> snd) |> List.length
    printfn "%A" vertexCount

    let mutable visited: bool array = Array.init  100 (fun i -> if i = 0 then true else false)
    printfn "%A" visited

    let mutable queue: int list = [0]

    let mutable result = false

    while (List.length queue > 0 && (not result)) do 
        //printfn "Queue: %A" queue

        // Pop off first element of queue
        let popID: int = List.head queue
        queue <- (List.tail queue)

        let popVertex: Vertex<'a,'b> = getVertex popID g
        let popVertexAdjacencyList: EdgeData<'E> list = popVertex |> snd
        let popAdjacentVerticiesIDs: (int * int) list = List.map (fun (_,_,v,(flow, cap)) -> (v,cap-flow)) popVertexAdjacencyList
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
    
    printfn "PARENT: %A" parent
    result