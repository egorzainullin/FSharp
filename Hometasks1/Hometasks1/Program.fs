// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec factorial n = 
    match n with 
    | 0 -> 1
    | _ -> n * factorial (n - 1)

let f5f = factorial 5

let fibonacci n = 
    let rec fibWithAccum n x y k =
        if k = n then y else fibWithAccum n y (x + y) (k + 1)
    fibWithAccum n 0 1 1

let reverseList list = 
    let rec reverse list result = 
        match list with
        | h::tail -> reverse tail (List.append [ h ] result) 
        | _ -> result
    reverse list []

let createListWithTwoPows m n =
    let rec twoIntoThisPower x =
        match x with
        | 0 -> 1
        | _ -> 2 * twoIntoThisPower (x - 1)
    [ n .. m + n] 
    |> List.map twoIntoThisPower

let powers = createListWithTwoPows 1 2

let reversed = reverseList [ 1 .. 4 ]

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
