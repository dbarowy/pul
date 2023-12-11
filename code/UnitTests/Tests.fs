namespace langTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Combinator
open AST
open Parser
open Graph
open GraphMaxFlowAlgorithm
open GraphCreationFunctions
open GraphPrettyPrint
open Evaluator


[<TestClass>]
type TestClass () =

    let rec constructGraph (g: Graph<string,int>) (i:int) (n:int): Graph<string,int> =
        // Recursiely add elements of input to g
        match i with
        | 0     -> g
        | _     -> 
            let str = "vertex" + sprintf "%i" n
            let newVal = i - 1
            let newN = n+ 1
            constructGraph (Graph.addVertex str g |> snd) newVal newN

    //////////////////////////// PARSER TESTS /////////////////////////////////
    
    [<TestMethod>]
    member this.parserTest () =
        let input = "Offer: Zach to Zurich at 3 PM on 12/8/2023 to 7 PM on 12/8/2023 with 2 seat \nRequest: Bob to Burlington at 5 AM on 1/4/1990 \nOffer: Melanie to Monterrey at 2 AM on 5/22/2000 to 10 PM on 5/31/2000 with 3 seats"
        let expected = [Offer ("Zach", ["Zurich"], {startTime = DateTime.Parse("3 PM 12/8/2023"); endTime = DateTime.Parse("7 PM 12/8/2023")}, 2);
                        Request ("Bob", ["Burlington"], {startTime = DateTime.Parse("5 AM 1/4/1990"); endTime = DateTime.Parse("5 AM 1/4/1990")});
                        Offer("Melanie", ["Monterrey"], {startTime = DateTime.Parse("2 AM 5/22/2000"); endTime = DateTime.Parse("10 PM 5/31/2000")}, 3)]
        let result = parse input
        match result with 
        | Some ast   -> Assert.AreEqual(expected,ast)
        | None       -> Assert.IsTrue false

    //////////////////////////// EVALUATOR HELPER TESTS /////////////////////////////////
    
    [<TestMethod>]
    member this.BFSTest1 () = 
        let graph = Graph.empty            
        let graph2 = constructGraph graph 3 0
        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd
        let finalGraph = (Graph.addEdge 0 1 2 2 graph3) |> snd

        let parent: int array = Array.init (finalGraph |> fst) (fun i -> -1)
        
        let result = BFS finalGraph 0 2 parent

        match result with 
        | true   -> Assert.IsTrue true
        | false  -> Assert.IsTrue false

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
        | false  -> Assert.IsTrue true
        | true   -> Assert.IsTrue false
    
    [<TestMethod>]
    member this.BFSTest3 () = 
        let graph = Graph.empty            
        let graph2 = constructGraph graph 6 0
        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd
        let graph4 = (Graph.addEdge 0 0 2 1 graph3) |> snd
        let graph5 = (Graph.addEdge 0 3 5 1 graph4) |> snd
        let graph6 = (Graph.addEdge 0 4 5 1 graph5) |> snd
        let finalGraph = (Graph.addEdge 0 2 4 1 graph6) |> snd

        let parent: int array = Array.init (finalGraph |> fst) (fun i -> -1)
        
        let result = BFS finalGraph 0 5 parent

        match result with 
        | true   ->  Assert.IsTrue true
        | false  ->  Assert.IsTrue false

    [<TestMethod>]
    member this.FordFulkersonTest1 () = 
        let graph = Graph.empty            
        let graph2 = constructGraph graph 3 0
        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd
        let finalGraph = (Graph.addEdge 0 1 2 2 graph3) |> snd
        
        let max_flow_result = (fordFulkerson finalGraph 0 2) |> fst

        match (max_flow_result = 1) with 
        | true   -> Assert.IsTrue true
        | false  -> Assert.IsTrue false

    [<TestMethod>]
    member this.FordFulkersonTest2 () = 
        let graph = Graph.empty            
        let graph2 = constructGraph graph 6 0
        let graph3 = (Graph.addEdge 0 0 1 1 graph2) |> snd
        let graph4 = (Graph.addEdge 0 0 2 2 graph3) |> snd
        let graph5 = (Graph.addEdge 0 3 5 3 graph4) |> snd
        let finalGraph = (Graph.addEdge 0 4 5 4 graph5) |> snd

        let max_flow_result = (fordFulkerson finalGraph 0 5) |> fst

        match (max_flow_result = 0) with 
        | true  -> Assert.IsTrue true
        | false   -> Assert.IsTrue false
    
    [<TestMethod>]
    member this.FordFulkersonTest3 () = 
        let graph = Graph.empty            
        let graph2 = constructGraph graph 6 0
        let graph3 = (Graph.addEdge 0 0 1 100 graph2) |> snd
        let graph4 = (Graph.addEdge 0 0 2 9 graph3) |> snd
        let graph5 = (Graph.addEdge 0 3 5 9 graph4) |> snd
        let graph6 = (Graph.addEdge 0 4 5 9 graph5) |> snd
        let finalGraph = (Graph.addEdge 0 2 4 2 graph6) |> snd

        let max_flow_result = (fordFulkerson finalGraph 0 5) |> fst

        match (max_flow_result = 2) with 
        | true   ->  Assert.IsTrue true
        | false  ->  Assert.IsTrue false

    //////////////////////////// EVALUATOR TESTS /////////////////////////////////
    
    [<TestMethod>]
    member this.EvalTest1 () = 
        let input =[Offer ("Abe", ["Arlington"], { startTime = DateTime.Parse("1/5/2023 2:00:00AM"); endTime = DateTime.Parse("1/5/2023 6:00:00AM") }, 2);
                    Request ("Bob", ["Boston"], { startTime = DateTime.Parse("1/3/2023 5:00:00AM"); endTime = DateTime.Parse("1/3/2023 5:00:00AM") });
                    Offer ("Cam", ["Chicago"], { startTime = DateTime.Parse("5/22/2023 2:00:00AM"); endTime = DateTime.Parse("5/24/2023 10:00:00AM") }, 4);
                    Request ("Dan", ["Denver"], { startTime = DateTime.Parse("11/27/2023 9:00:00PM"); endTime = DateTime.Parse("11/27/2023 9:00:00PM") });
                    Request ("Echo", ["Chicago"], { startTime = DateTime.Parse("5/22/2023 10:00:00AM"); endTime = DateTime.Parse("5/22/2023 10:00:00AM") });
                    Request ("Fay", ["Chicago"], { startTime = DateTime.Parse("5/22/2023 10:00:00AM"); endTime = DateTime.Parse("5/22/2023 10:00:00AM") });
                    Request ("Gabe", ["Arlington"; "Chicago"], { startTime = DateTime.Parse("1/1/2023 5:00:00AM"); endTime = DateTime.Parse("5/22/2023 5:00:00AM") });
                    Request ("Hay", ["Arlington"], { startTime = DateTime.Parse("1/1/2023 1:00:00AM"); endTime = DateTime.Parse("1/9/2023 11:00:00PM") })]
        let result = eval input
        let expectedResult = 
            "MATCHED REQUESTS:\n" +
            "\tRequest: Echo to Chicago at 10 AM on 5/22/2023 filled by Offer: Cam to Chicago at 2 AM on 5/22/2023 to 10 AM on 5/24/2023\n" +
            "\tRequest: Fay to Chicago at 10 AM on 5/22/2023 filled by Offer: Cam to Chicago at 2 AM on 5/22/2023 to 10 AM on 5/24/2023\n" +
            "\tRequest: Gabe to Arlington, Chicago at 5 AM on 1/1/2023 to 5 AM on 5/22/2023 filled by Offer: Abe to Arlington at 2 AM on 1/5/2023 to 6 AM on 1/5/2023\n" +
            "\tRequest: Hay to Arlington at 1 AM on 1/1/2023 to 11 PM on 1/9/2023 filled by Offer: Abe to Arlington at 2 AM on 1/5/2023 to 6 AM on 1/5/2023\n\n" +
            "UNMATCHED REQUESTS:\n" +
            "\tRequest: Bob to Boston at 5 AM on 1/3/2023\n" +
            "\tRequest: Dan to Denver at 9 PM on 11/27/2023\n"
        match (result = expectedResult) with
        | true   ->  Assert.IsTrue true
        | false  ->  Assert.IsTrue false