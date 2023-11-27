open QuikGraph
open QuikGraph.Algorithms
open QuikGraph.Graphviz

[<EntryPoint>]
let main (args) =  
    printfn "Hello from F#"

    let v1 = 5
    let v2 = 7
    let v3 = 10
    let e1 = SEquatableEdge(5, 7)
    let e2 = SEquatableEdge(5, 10)
    let e3 = SEquatableTaggedEdge(5, 7, "First")
    let e4 = SEquatableTaggedEdge(5, 10, "Second")
    let edges = [e3;e4]
    let vertices: 'a list = []
    let graph = edges.ToBidirectionalGraph()
    let graph2 = vertices.ToBidirectionalGraph()
    let boolean = graph2.AddVertex(v1)
    let boolean = graph2.AddVertex(v2)
    let boolean = graph2.AddVertex(v3)
    let boolean = graph2.AddEdge(e1)
    let boolean = graph2.AddEdge(e2)
    //let something = graph2 |> fst
    let graphviz = GraphvizAlgorithm(graph2)
    let string2: string = graphviz.Generate()
    printfn "%A" string2
    let verticesComplete2 = graph2.Vertices
    printfn "%A" verticesComplete2  

    let graphviz = GraphvizAlgorithm(graph)
    let string: string = graphviz.Generate()
    printfn "%A" string
    let verticesComplete = graph.Vertices
    printfn "%A" verticesComplete 
    let edgesComplete = graph.Edges
    printfn "%A" edgesComplete

    // Source to offer
    let e1 = SEquatableTaggedEdge(0, 1, 2)
    let e2 = SEquatableTaggedEdge(0, 2, 1)

    // Offers to requests
    let e3 = SEquatableTaggedEdge(1, 3, 1)
    let e4 = SEquatableTaggedEdge(1, 4, 1)
    let e5 = SEquatableTaggedEdge(2, 3, 1)
    let e6 = SEquatableTaggedEdge(2, 4, 1)

    // Reuests to sink
    let e7 = SEquatableTaggedEdge(3, 5, 1)
    let e8 = SEquatableTaggedEdge(4, 5, 1)

    let edges = [e1;e2;e3;e4;e5;e6;e7;e8]
    let graph = edges.ToBidirectionalGraph()
    let edgesComplete = graph.Edges
    let graphviz = GraphvizAlgorithm(graph)
    let string: string = graphviz.Generate()
    printfn "%A" string
    let verticesComplete = graph.Vertices
    printfn "%A" verticesComplete 
    
    printfn "%A" edgesComplete 

    let capacityFunc (edge: 'a * 'b * 'c) : 'c =
        match edge with
        | (x,y,z) -> z

    //let capacity2: System.Func<'a> = System.Func (capacityFunc edge)

    
    let flowPredecessors (vertex: int) : SEquatableTaggedEdge<int,int> =
        let edgesComplete: System.Collections.Generic.IEnumerable<SEquatableTaggedEdge<int,int>> ref = ref graph.Edges
        let doesOutEdgeExist: bool = graph.TryGetInEdges(vertex, edgesComplete)
        graph.InEdge(vertex, 0)
        //match doesOutEdgeExist with
        //| true -> 
        //| false -> None

    let edgeFactory (source,target) : SEquatableTaggedEdge<'a,int> = SEquatableTaggedEdge(source, target, 0)
    //let edgeFactory2 = EdgeFactory(edgeFactory)

    //graph.MaximumFlow

    //printfn "%A" something
    

    0