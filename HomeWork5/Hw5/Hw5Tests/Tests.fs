module Hw5Test

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
    "()[][{}]" |> isCorrectBracketSeq |> should equal true
    "" |> isCorrectBracketSeq |> should equal true
    "([)]" |> isCorrectBracketSeq |> should equal false
    "((()))" |> isCorrectBracketSeq |> should equal true

[<Test>]
let ``complex brackets checking``() =
    "(()[]){((()))}[[()]]" |> isCorrectBracketSeq |> should equal true

open Telephone

let l = [("89", "Alex"); ("89", "Susan"); ("56", "Max")]

[<Test>]
let ``test adding``() =
    let func phone name list = List.length (addToBase phone name list) = (List.length list) + 1
    Check.QuickThrowOnFailure func

[<Test>]
let ``test finding by phone``() =
    findNameByPhone "89" l |> List.sort |> should equal ["Alex"; "Susan"]
    findNameByPhone "89" [] |> should equal []

[<Test>]
let ``test finding by name``() = 
    let list = l @ [("99", "Susan")]
    findPhoneByName "Susan" list |> List.sort |> should equal ["89"; "99"]
    findNameByPhone "Someone" [] |> should equal []

[<Test>]
let ``saving & loading testing``() =
    let func (l : (string * string) list) = 
        saveToFile l
        let list = readFromFile "output.dat"
        List.sort list = List.sort l
    Check.QuickThrowOnFailure func