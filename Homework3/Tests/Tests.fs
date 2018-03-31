module Test.``Tests``

open FsUnit
open NUnit.Framework
    
open Homework3
open Numbers
    
[<Test>]
let ``Check that primes's seq is Ok``() = 
    let list = primes |> Seq.take 36 |> Seq.toList
    list.[0] |> should equal 2
    list.[1] |> should equal 3
    list.[2] |> should equal 5
    list.[35] |> should equal 151

[<Test>]
let ``Counting even in list``() =
    let list = [1 .. 4]
    countEvenWithMap list |> should equal 2
    countEvenWithFilter list |> should equal 2
    countEvenWithFold list |> should equal 2

[<Test>]
let ``Counting even in list without even numbers``() = 
    let list = [1; 3; 5]
    countEvenWithMap list |> should equal 0
    countEvenWithFilter list |> should equal 0
    countEvenWithFold list |> should equal 0

[<Test>]
let ``Counting even in empty list``() =
    let list = []
    countEvenWithMap list |> should equal 0
    countEvenWithFilter list |> should equal 0
    countEvenWithFold list |> should equal 0

open Tree

[<Test>]
let ``Check that map is Ok``() = 
    let tree = Tree(3, Tree(1, Tip(2), Tip(3)), Tree(2, Tip(1), Tip(3)))
    let f = fun x -> 2 * x
    let newTree = map f tree
    newTree |> should equal (Tree(6, Tree(2, Tip(4), Tip(6)), Tree(4, Tip(2), Tip(6))))

[<Test>]
let ``Check empty & one element tree mapping``() =
    let f = (+) 1
    let oneElementTree = Tip(1)
    let oneElementTreeAfterMapppig = map f oneElementTree  
    oneElementTreeAfterMapppig |> should equal (Tip(2))

[<Test>]
let ``Check mapping into another type``() =
    let tree = Tree(3, Tree(1, Tip(2), Tip(3)), Tree(2, Tip(1), Tip(3)))
    let f x = x.ToString()
    let newTree = map f tree
    newTree |> should equal (Tree("3", Tree("1", Tip("2"), Tip("3")), Tree("2", Tip("1"), Tip("3"))))

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
