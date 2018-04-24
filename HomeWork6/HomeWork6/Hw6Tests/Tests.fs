module HwTests

open NUnit.Framework
open FsUnit

open Homework6
open BinaryTree

let mutable tree = new Tree<int>()

[<SetUp>]
let ``set up tests``() =
    tree <- new Tree<int>()

[<Test>]
let ``test adding``() =
    tree.Add(2)
    tree.Add(3)
    tree.Count |> should equal 2

[<Test>]
let ``test enumerator``() =
    tree.Add(2)
    tree.Add(3)
    tree.Add(1)
    let l = [for i in tree do yield i]
    l |> List.sort |> should equal [1; 2; 3]

[<Test>]
let ``test To seq``() = 
    tree.Add(2)
    tree.Add(3)
    tree.Add(1)
    tree.GetSeq() |> Seq.toList |> List.sort |> should equal [1; 2; 3]

[<Test>]
let ``test count``() =
    tree.Count |> should equal 0
    tree.Add(5) 
    tree.Count |> should equal 1

[<Test>]
let ``test removing``() =
    for i in [1 .. 10] do tree.Add(i)
    tree.Remove(5)
    tree.Count |> should equal 9
    (fun () -> tree.Remove(11)) |> should throw typeof<System.ArgumentException>

[<Test>]
let ``test isContatins``() =
    for i in [1 .. 10] do tree.Add(i)
    tree.Remove(5)
    tree.IsContains(5) |> should equal false
    tree.IsContains(1) |> should equal true
