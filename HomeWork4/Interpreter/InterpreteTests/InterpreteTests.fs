module InterpreterTests

open NUnit.Framework
open FsUnit
open Interprete

[<Test>]
let ``when should not change``() =
    let term = Application(Variable("x"), Variable("y"))
    let transformed = betaTransform term
    transformed |> should equal term

[<Test>]
let ``IdId is id``() =
    let term = Lambda("x", Application(Lambda("x", Variable("x")), Variable("x")))
    let transformed = betaTransform term
    transformed |> should equal (Lambda("x", Variable("x")))

[<Test>]
let ``K id term is id``() = 
    let term = Application(Application(Lambda("x", Lambda("y", Variable("x"))), Lambda("z", Variable("z"))), Variable("m"))
    term |> betaTransform |> betaTransform |> betaTransform |> should equal (Lambda("z", Variable("z")))

[<Test>]
let ``test basic substitute``() =
    let substitution = Variable("y")
    let term1 = Variable("x")
    (substitute "x" substitution term1) |> should equal (Variable("y"))
    let term2 = Lambda("x", Variable("x"))
    (substitute "x" substitution term2) |> should equal term2

[<Test>]
let ``test complex substitute``() = 
    let substitution = Variable("z")
    let term = Lambda("y", Application(Variable("x"), Variable("y")))
    (substitute "x" substitution term) |> should equal (Lambda("y", Application(Variable("z"), Variable("y"))))

[<Test>]
let ``check that finding free vars is OK``() =
    Application(Lambda("x", Variable("x")), Variable("y"))
    |> findFreeVars
    |> should equal (Set.singleton "y")

[<Test>]
let ``check that renameing is OK``() = 
    substitute "y" (Variable("x")) (Application(Lambda("x", Application(Variable("a"), Variable("y"))), Variable("y")))
    |> should equal (Application(Lambda ("b",Application (Variable("a"), Variable("x"))), Variable("x")))

[<Test>]
let ``check that betatrnasfrom with renaming os OK``() =
    Application(Lambda("x", Lambda("y", Variable("x"))), Variable("y"))
    |> betaTransform
    |> should equal (Lambda("a", Variable("y")))