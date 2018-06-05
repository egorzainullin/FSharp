open System

let convertToInt (str : string) =
    match Int32.TryParse str with
    | true, num -> Some(num)
    | _ -> None

type StringConvertBuilder() =
     member this.Bind(x, f) =
        let converted = convertToInt x
        match converted with
        | Some(n) -> f n
        | None -> None
     member this.Return(x) = Some(x)    

let convertToExactPrecise (x : float) (numberOfDigits : int) = 
    Math.Round(x, numberOfDigits)

type PreciseBuilder(precise) = 
     new() = PreciseBuilder(0)
     member this.Bind(x, f) = convertToExactPrecise x precise |> f
     member this.Return(x) = convertToExactPrecise x precise   

let preciseCalculation x = new PreciseBuilder(x)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
