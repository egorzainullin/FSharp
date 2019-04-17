module KrkaTest

open FsUnit
open NUnit.Framework
open Krka1
open System
open FsUnitTyped.TopLevelOperators

[<Test>]
let ``average of simular sin is ok``() =
    let list1 = [0.0; 0.0; 0.0; 0.0]
    (findAverageSin list1) |> should equal 0.0
    let x = Math.PI / 2.0
    let list2 = [x; x; x]
    (findAverageSin list2) |> should equal 1.0

[<Test>]
let ``average of sin pi/ 2 pi/2 0 is 2/3``() = 
    let x = Math.PI / 2.0
    let list = [x; x; 0.0]
    (findAverageSin list) |> should equal (2.0 / 3.0)

[<Test>]
let ``test correct distance calculating``() =
    let tree = Tree(1, [Null; Null]): Tree<int>
    let tree2 = Tree(2, [tree; Null])
    (findShortest tree2) |> should equal 1
    (fun _ -> findShortest Null |> ignore) |> shouldFail
    let tree3 = Tree(3, [tree2; tree2])
    (findShortest tree3) |> should equal 2