﻿module Hw5Test

open FsCheck
open Hw5
open Hw5.PointFree
open Hw5.Brackets
open NUnit.Framework
open FsUnit

let f x l = List.map (fun y -> y * x) l

[<Test>]
let ``check equality 1``() = 
    let func x l = (f x l) = (f1 x l)
    Check.QuickThrowOnFailure func

[<Test>]
let ``check equality 2``() = 
    let func x l = (f x l) = (f2 x l)
    Check.QuickThrowOnFailure func

[<Test>]
let ``check equality 3``() = 
    let func x l = (f x l) = (f3 x l)
    Check.QuickThrowOnFailure func

[<Test>]
let ``check brackets seq is OK``() =
    "()[][{}]" |> IsCorrectBracketSeq |> should equal true
    "" |> IsCorrectBracketSeq |> should equal true
    "([)]" |> IsCorrectBracketSeq |> should equal false
    "((()))" |> IsCorrectBracketSeq |> should equal true

[<Test>]
let ``complex brackets checking``() =
    "(()[]){((()))}[[()]]" |> IsCorrectBracketSeq |> should equal true