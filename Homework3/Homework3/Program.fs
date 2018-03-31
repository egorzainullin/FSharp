namespace Homework3

module Numbers =

        open Microsoft.FSharp.Core.Option

        let countEvenWithMap list =
            let func x = if x % 2 = 0 then 1 else 0
            List.map func list |> List.sum

        let countEvenWithFilter list = 
            List.filter (fun x -> x % 2 = 0) list |> List.length

        let countEvenWithFold list =
            let newState state x = if x % 2 = 0 then state + 1 else state
            List.fold newState 0 list

        let primes =  
            let isPrime (x : int) seq = Seq.tryFind (fun i -> x % i = 0) seq |> isNone
            seq {
                yield 2
                let getPrime listOfPrimes i = 
                    match i with
                    | Some(x) -> if isPrime x listOfPrimes then Some(x) else None
                    | _ -> None
                let unionWithIfNotNull i list = 
                    match i with
                    | None -> list
                    | Some(x) -> x :: list 
                let res = (Seq.unfold (fun (state, i) -> Some(getPrime state i, ((unionWithIfNotNull i state), Some(i.Value + 1)))) ([2], Some(3))
                |> Seq.filter(fun i -> i <> None)) |> Seq.choose id 
                yield! res
                }

module Program =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
    