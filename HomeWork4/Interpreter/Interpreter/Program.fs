// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Interprete

type Term = 
    | Variable of string
    | Application of Term * Term
    | Lambda of string * Term

let rec findFreeVars term =
    match term with
    | Variable(x) -> Set.singleton x
    | Application(t1, t2) -> Set.union (findFreeVars t1) (findFreeVars t2)
    | Lambda(x, t) -> Set.difference (findFreeVars t) (Set.singleton x)

let rec substitute variable substitution term =
    let freeVars = findFreeVars substitution
    let freeVarsFromTerm = findFreeVars term
    let allFreeVars = Set.union freeVars freeVarsFromTerm
    let findNameToRename set =
        let alphabet = ['a' .. 'z'] |> List.map (fun x -> x.ToString())
        let rec findNameToRename' counter =
            let f c x = if c = 0 then x else x + c.ToString() 
            let suitable = alphabet |> List.map (f counter) 
                           |> List.filter ( fun x -> not (Set.contains x set))
            if List.isEmpty suitable then findNameToRename' (counter + 1)
            else suitable.[0]
        findNameToRename' 0              
    match term with 
    | Variable(x) -> if (x = variable) then substitution else Variable(x)
    | Application(t1, t2) -> Application(substitute variable substitution t1, substitute variable substitution t2)
    | Lambda(var, term) -> 
        if (var <> variable) then 
            if Set.contains var freeVars then
                let varReplacement = findNameToRename allFreeVars
                let newLambda = Lambda(varReplacement ,(substitute var (Variable(varReplacement)) term))
                substitute variable substitution newLambda 
            else Lambda(var, substitute variable substitution term)
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
    findFreeVars <| Application(Lambda("x", Variable("x")), Variable("y"))
    |> printfn "%A"
    substitute "y" (Variable("x")) (Application(Lambda("x", Application(Variable("a"), Variable("y"))), Variable("y")))
    |> printfn "%A"
    let t = Application(Lambda("x", Application(Variable("a"), Variable("y"))), Variable("y"))
    printfn "%A" t
    betaTransform t
    |> printfn "%A"
    let term = Application(Lambda("x", Lambda("y", Variable("x"))), Variable("y"))
    printfn "%A" term
    printfn "%A" <| betaTransform term
    printfn "%A" argv
    0 // return an integer exit code
