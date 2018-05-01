namespace Homework6

module Net =
    
    open System
    
    /// <summary> 
    /// Abstraction of computer
    /// </summary>
    type Machine(os : string, possibility : double, isInfected : bool) =

        // Is this computer infected
        let mutable isInfectedMutable = isInfected

        // Type of OS
        member val TypeOfOs = os 

        /// <summary>
        /// Possibility of infection
        /// </summary>
        member val Possibility = possibility

        /// <summary>
        /// Gets is this computer infected
        /// </summary>
        member this.IsInfected with get() = isInfectedMutable

        /// <summary>
        /// Sets this computer infected
        /// </summary>
        member this.SetInfected() = isInfectedMutable <- true

    // Abstraction of net of computers
    type Net(arrayOfComputers : Machine list, graph : (bool list) list, random : Random) =

        // Computers in the net
        let mutable computers = List.toArray arrayOfComputers

        // Number of computers
        let length = arrayOfComputers.Length
        
        // Maps array of computers, map : this.IsInfected 
        let convertToBool() = Array.map (fun (x : Machine) -> x.IsInfected) computers

        /// <summary>
        /// Gets which computers are infected
        /// </summary>
        member this.InfectedComputers with get() = convertToBool()

        /// <summary>
        /// Emulates a new turn
        /// </summary>
        member this.NewTurn() = 
            let state = convertToBool()
            let infect i x = 
                if state.[i] then
                    let valueFromRandom = random.NextDouble()
                    if valueFromRandom <= computers.[x].Possibility then computers.[x].SetInfected()
            let infectList i list = list |> List.indexed
                                    |> List.filter snd |> List.map fst |> List.map (infect i)
            List.mapi infectList graph |> ignore
        
        /// <summary>
        /// Checks is that end of process
        /// </summary>
        member this.IsEndOfProcess() = 
            let temp = convertToBool()
            let isInfected i = temp.[i]
            let probabilityMoreThanZeroAndNotInfected i = (computers.[i].Possibility > 0.0) && not (isInfected i)
            let z = graph |> List.indexed |> List.filter (fst >> isInfected)
                    |> List.map snd |> List.map (List.indexed) 
                    |> List.map (List.filter (snd >> not))
                    |> List.map (List.filter (fst >> probabilityMoreThanZeroAndNotInfected))
                    |> List.filter (List.isEmpty >> not) |> List.isEmpty
            z     
                  
        /// <summary>
        /// Initializes a new instance of <see cref="Net">
        /// </summary>
        new(arrayOfComputers, graph) = Net(arrayOfComputers, graph, new Random())
            
        