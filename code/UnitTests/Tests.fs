namespace langTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST

[<TestClass>]
type TestClass () =
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
