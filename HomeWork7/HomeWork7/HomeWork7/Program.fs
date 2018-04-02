﻿open System

let convertToInt (str : string) =
    let rec convertToInt' (str : string) result i =
        if str.Length = i then Some(result)
        else match str.[i] with
             | '0' -> convertToInt' str (result * 10) (i + 1)
             | '1' -> convertToInt' str (result * 10 + 1) (i + 1)
             | '2' -> convertToInt' str (result * 10 + 2) (i + 1)
             | '3' -> convertToInt' str (result * 10 + 3) (i + 1)
             | '4' -> convertToInt' str (result * 10 + 4) (i + 1)
             | '5' -> convertToInt' str (result * 10 + 5) (i + 1)
             | '6' -> convertToInt' str (result * 10 + 6) (i + 1)
             | '7' -> convertToInt' str (result * 10 + 7) (i + 1)
             | '8' -> convertToInt' str (result * 10 + 8) (i + 1)
             | '9' -> convertToInt' str (result * 10 + 9) (i + 1)
             | _ -> None
    convertToInt' str 0 0

type StringConvertBuilder() =
     member this.Bind(x, f) =
        let converted = convertToInt x
        match converted with
        | Some(n) -> f n
        | None -> None
     member this.Return(x) = Some(x)    

let converter = new StringConvertBuilder()

let z = converter { 
    let! x = "1"
    let! y = "2"        
    let z = x + y
    return z }

let z1 = converter { 
    let! x = "1"
    let! y = "a"        
    let z = x + y
    return z }

printfn "%A" z
printfn "%A" (z1 = None)

let convertToExactPrecise x numberOfDigits = 
    let z = (float) (pown 10 numberOfDigits)
    x * z |> round |> (/) <| z

printfn "%A" (convertToExactPrecise 10.0101 2)

type PreciseBuilder(precise) = 
     new() = PreciseBuilder(0)
     member this.Bind(x, f) = convertToExactPrecise x precise |> f
     member this.Return(x) = convertToExactPrecise x precise   

let preciseCalculation x = new PreciseBuilder(x)

let n = preciseCalculation 3{
    let! a = 2.0 / 12.0
    let! b = 3.5
    return a / b
    }

printfn "%A" n

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code