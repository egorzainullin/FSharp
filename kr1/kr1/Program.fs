// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Program

module Priority =

    /// <summary>
    /// Class that supposed to be queue with priorities
    /// </summary>
    type Queue<'T>() =
    
        /// Internal list that contains elements of queue
        let mutable list = []

        /// <summary>
        /// Gets count of elements
        /// </summary>
        member this.Count with get() = List.length list

        /// <summary>
        /// Puts new element in queue
        /// </summary>
        /// <param name="value">Value to put</param>
        /// <param name="prior">Priority of value</param>
        member this.Enequeue (value : 'T) prior =
            let rec putValueInRightPlace prev rest =
                match rest with
                | [] -> (value, prior) :: prev
                | (headValue, headPrior) :: tail -> if headPrior <= prior then (List.rev rest) @ ((value, prior) :: prev)  
                                                    else putValueInRightPlace ((headValue, headPrior) :: prev) tail
            list <- List.rev (putValueInRightPlace [] (list)) 
    
        /// <summary>
        /// Gets element with highest priority and removes it
        /// </summary>
        /// <exception cref="InvalidOperationException">Throws when queue is empty</exception>
        member this.Dequeue() =
            if list.IsEmpty then raise (System.InvalidOperationException("queue is empty"))
            let temp = list.Head
            list <- list.Tail
            temp |> fst

        /// <summary>
        /// Returns true, if queue is empty, otherwise returns false
        /// </summary>
        member this.IsEmpty() = list.IsEmpty

    [<EntryPoint>]
    let main argv =
        let queue = Queue<int>()
        for i in [1 .. 50] do queue.Enequeue i i
        queue.Enequeue 101 101
        for i in (List.rev [51 .. 100]) do queue.Enequeue i i
        for i in [1 .. 100] do printfn "%A" (queue.Dequeue())
        printfn "%A" argv
        0 // return an integer exit code