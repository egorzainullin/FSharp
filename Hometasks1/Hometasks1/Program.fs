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
    let rec getResutList numberToAdd k = 
        match k with
        | -1 -> [] 
        | _ -> numberToAdd :: getResutList (numberToAdd * 2) (k - 1)
    let rec powerInto n =
        if n = 0 then 1
        else 
        let halfPower = powerInto (n / 2)
        match n % 2 with
        | 0 -> halfPower * halfPower
        | _ -> 2 * halfPower * halfPower
    getResutList (powerInto n) m

let powers = createListWithTwoPows 2 2

let reversed = reverseList [ 1 .. 5 ]

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
