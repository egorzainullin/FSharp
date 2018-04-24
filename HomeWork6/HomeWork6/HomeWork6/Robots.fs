namespace HomeWork6

module Robots =
    open System
    
    type Machine(os : string, possibility : double, isInfected : bool) =

        let mutable isInfectedMutable = isInfected

        member val TypeOfOs = os 

        member val Possibility = possibility

        member this.IsInfected with get() = isInfectedMutable

        member this.SetInfected() = isInfectedMutable <- true

    type Net(arrayOfComputers : Machine list, graph : (bool list) list, random : Random) =

        let mutable computers = List.toArray arrayOfComputers

        let length = arrayOfComputers.Length

        let convertToBool() = Array.map (fun (x : Machine) -> x.IsInfected) computers

        member this.InfectedComputers with get() = convertToBool()

        member this.NewTurn() = 
            let state = convertToBool()
            let infect i x = 
                if state.[i] then
                    let valueFromRandom = random.NextDouble()
                    if valueFromRandom <= computers.[x].Possibility then computers.[x].SetInfected()
            let infectList i list = list |> List.indexed
                                    |> List.filter snd |> List.map fst |> List.map (infect i)
            List.mapi infectList graph |> ignore

        member this.IsEndOfProcess() = 
            let temp = convertToBool()
            let isInfected i = temp.[i]
            let probabiliTyNoreThanZero i = computers.[i].Possibility > 0.0
            graph |> List.indexed |> List.filter (fst >> isInfected)
            |> List.map snd |> List.map (List.indexed) 
            |> List.map (List.filter (snd >> not))
            |> List.map (List.filter (fst >> probabiliTyNoreThanZero))
            |> List.filter List.isEmpty |> List.isEmpty |> not
                    

        new(arrayOfComputers, graph) = Net(arrayOfComputers, graph, new Random())
            
        