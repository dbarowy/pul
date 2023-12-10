namespace langTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST
open Graph
open Combinator
open GraphMaxFlowAlgorithm
open GraphCreationFunctions


[<TestClass>]
type TestClass () =

    let rec constructGraph (g: Graph<string,int>) (i:int) (n:int): Graph<string,int> =
        // Recursiely add elements of input to g
        match i with
        |0     -> g
        |_     -> 
            let str = "vertex" + sprintf "%i" n
            let newVal = i - 1
            let newN = n+ 1
            constructGraph (Graph.addVertex str g |> snd) newVal newN
    
    [<TestMethod>]
    member this.parserTest () =
        let input = "Offer: Zach to Zurich at 3 PM on 12/8/2023 to 7 PM on 12/8/2023 with 2 seat \nRequest: Bob to Burlington at 5 AM on 1/4/1990 \nOffer: Melanie to Monterrey at 2 AM on 5/22/2000 to 10 PM on 5/31/2000 with 3 seats"
        let expected = [Offer ("Zach", ["Zurich"], {startTime = DateTime.Parse("3 PM 12/8/2023"); endTime = DateTime.Parse("7 PM 12/8/2023")}, 2);
                        Request ("Bob", ["Burlington"], {startTime = DateTime.Parse("5 AM 1/4/1990"); endTime = DateTime.Parse("5 AM 1/4/1990")});
                        Offer("Melanie", ["Monterrey"], {startTime = DateTime.Parse("2 AM 5/22/2000"); endTime = DateTime.Parse("10 PM 5/31/2000")}, 3)]
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected,ast)
        |None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.BFSTest1 () = 
        let graph = Graph.empty            

        let graph2 = constructGraph graph 3 0

        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd

        let finalGraph = (Graph.addEdge 0 1 2 2 graph3) |> snd

        let parent: int array = Array.init (finalGraph |> fst) (fun i -> -1)
        
        let result = BFS finalGraph 0 2 parent

        match result with 
        |true -> Assert.IsTrue true
        |false->
            Assert.IsTrue false

    [<TestMethod>]
    member this.BFSTest2 () = 
        let graph = Graph.empty            

        let graph2 = constructGraph graph 6 0

        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd

        let graph4 = (Graph.addEdge 0 0 2 1 graph3) |> snd

        let graph5 = (Graph.addEdge 0 3 5 1 graph4) |> snd

        let finalGraph = (Graph.addEdge 0 4 5 1 graph5) |> snd

        let parent: int array = Array.init (finalGraph |> fst) (fun i -> -1)
        
        let result = BFS finalGraph 0 5 parent

        match result with 
        |false-> Assert.IsTrue true
        |true->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.BFSTest3 () = 
        let graph = Graph.empty            

        let graph2 = constructGraph graph 6 0

        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd

        let graph4 = (Graph.addEdge 0 0 2 1 graph3) |> snd

        let graph5 = (Graph.addEdge 0 3 5 1 graph4) |> snd

        let graph6 = (Graph.addEdge 0 4 5 1 graph5) |> snd

        let finalGraph = (Graph.addEdge 0 2 4 1 graph6) |> snd

        printfn "%A" finalGraph

        let parent: int array = Array.init (finalGraph |> fst) (fun i -> -1)
        
        let result = BFS finalGraph 0 5 parent

        match result with 
        |true   ->  Assert.IsTrue true
        |false  ->  Assert.IsTrue false