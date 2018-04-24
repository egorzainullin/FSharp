module Tests

open NUnit.Framework
open FsUnit
open Program.Priority    
    
let mutable queue = new Queue<int>()

[<SetUp>]
let ``set up tests``() = queue <- new Queue<int>()

[<Test>]
let ``test isEmpty``() =
    queue.IsEmpty() |> should equal true
    queue.Enequeue 1 1
    queue.IsEmpty() |> should equal false

[<Test>]
let ``test Eneque is OK``() =
    queue.Count |> should equal 0
    queue.Enequeue 1 1
    queue.Enequeue 2 2
    queue.Count |> should equal 2
    queue.Enequeue 0 -1
    queue.Count |> should equal 3
    
[<Test>]
let ``test Dequeue is OK``() =
    queue.Enequeue 1 1
    queue.Enequeue 3 100
    queue.Enequeue 2 2
    let value = queue.Dequeue() 
    queue.Count |> should equal 2
    value |> should equal 3

[<Test>]
let ``test exception``() =
    (fun () -> queue.Dequeue() |> ignore) |> should throw (typeof<System.InvalidOperationException>)

[<Test>]
let ``complextesting``() =
    for i in [1 .. 50] do queue.Enequeue i i
    for i in (List.rev [51 .. 100]) do queue.Enequeue i i
    let list = [for i in [1 .. 100] -> queue.Dequeue()]
    list |> should equal (List.rev [1 .. 100])