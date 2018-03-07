module Test.``Tests``

open FsUnit
open NUnit.Framework
    
open Homework3
open Numbers
    
[<Test>]
let ``Check that primes's seq is Ok``() = 
    let list = primes |> Seq.take 5 |> Seq.toList
    list.[0] |> should equal 2
    list.[1] |> should equal 3
    list.[2] |> should equal 5

[<Test>]
let ``Counting even in list``() =
    let list = [1 .. 4]
    countEven1 list |> should equal 2
    countEven2 list |> should equal 2
    countEven3 list |> should equal 2

open Tree

[<Test>]
let ``Check that map is Ok``() = 
    let tree = Tree(3, Tree(1, Tip(2), Tip(3)), Tree(2, Tip(1), Tip(3)))
    let f = fun x -> 2 * x
    let newTree = map f tree
    newTree |> should equal (Tree(6, Tree(2, Tip(4), Tip(6)), Tree(4, Tip(2), Tip(6))))

[<Test>]
let ``Calculate Expression``() =
    let expr1 = Divide(Plus(Operand(3), Operand(6)), Operand(3))
    let expr2 = Multiply(Operand(2), Minus(Operand(3), Operand(1)))
    calculate expr1 |> should equal 3
    calculate expr2 |> should equal 4

[<Test>]
let ``Divide by zero exception``() =
    let expr = Divide(Operand(2), Operand(0))
    (fun () -> calculate expr |> ignore) |> should throw typeof<System.DivideByZeroException>
