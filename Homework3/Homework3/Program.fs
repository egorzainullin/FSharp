namespace Homework3

module Numbers =

        open Microsoft.FSharp.Core.Option

        let countEven1 list =
            let func x = if x % 2 = 0 then 0 else 1
            List.map func list |> List.sum

        let countEven2 list = 
            List.filter (fun x -> x % 2 = 0) list |> List.length

        let countEven3 list =
            let newState state x = if x % 2 = 0 then state + 1 else state
            List.fold newState 0 list

        let primes =  
            let isPrime x seq = Seq.tryFind (fun i -> x % i = 0) seq |> isNone
            seq {
                yield 2
                let getPrime listOfPrimes i = 
                    if isPrime i listOfPrimes then i
                    else -1
                let unionWithIfNotNull i list = 
                    match i with
                    | -1 -> list
                    | x -> x :: list
                let res = (Seq.unfold (fun (state, i) -> Some(getPrime state i, ((unionWithIfNotNull i state), i + 1))) ([2], 3)
                |> Seq.filter(fun i -> i <> -1))
                yield! res
                }

module Program =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
    