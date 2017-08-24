module FSharp.EdIlyin.Core.Tests

open FSharp.EdIlyin.Core
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
    let x = Json.Encode.string null |> Json.Encode.encode
    let result = 42
    printfn "%i" result
    Assert.AreEqual(42,result)
