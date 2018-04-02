// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Interprete

type Term = 
    | Variable of string
    | Application of Term * Term
    | Lambda of string * Term

let rec substitute variable substitution term =
    match term with 
    | Variable(x) -> if (x = variable) then substitution else Variable(x)
    | Application(t1, t2) -> Application(substitute variable substitution t1, substitute variable substitution t2)
    | Lambda(var, term) -> if (var <> variable) then Lambda(var, substitute variable substitution term)
                                                else Lambda(var, term)

let betaTransform term =
    let rec betaTransform' term = 
        match term with
        | Application(Lambda(var, term) , t2) -> (substitute var t2 term, true)
        | Application(t1, t2) -> 
                                 let (beta1, isChanged1) = betaTransform' t1
                                 if isChanged1 then (Application(beta1, t2), true)
                                 else let (beta2, isChanged2) = betaTransform' t2 in 
                                        if isChanged2 then (Application(t1, beta2), true)
                                        else (Application(t1, t2), false) 
        | Lambda(var, term) -> let (transformed, isChanged) = betaTransform' term
                               (Lambda(var, transformed), isChanged)
        | Variable(x) -> (Variable(x), false)
    betaTransform' term |> fst

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
