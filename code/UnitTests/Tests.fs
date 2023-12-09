namespace langTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.parserTest () =
        let input = "Offer: Zach to Zurich at 3-7 on 12/8 with 2 seat \nRequest: Bob to Burlington at 5 on 1/4 \nOffer: Melanie to Monterrey at 2-10 on 5/22-5/31 with 3 seats"
        let expected = [Offer ("Zach", ["Zurich"], { startHour = 3; endHour = 7 }, { startDate = { month = 12; day = 8 }; endDate = { month = 12; day = 8 } }, 2);
                        Request ("Bob", ["Burlington"], { startHour = 5; endHour = 5 }, { startDate = { month = 1; day = 4 };endDate = { month = 1; day = 4 } });
                        Offer("Melanie", ["Monterrey"], { startHour = 2; endHour = 10 }, { startDate = { month = 5;day = 22 };endDate = { month = 5;day = 31 }}, 3)]
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected,ast)
        |None ->
            Assert.IsTrue false
