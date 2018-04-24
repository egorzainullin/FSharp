namespace Homework6

module BinaryTree = 
    open System.Collections.Generic
    open System.Collections
    open System
    open HomeWork6
    
    type internal TreeElement<'T> = 
        | Null
        | TreeElement of 'T * TreeElement<'T> * TreeElement<'T>

    /// <summary>
    /// IEnumerable collection that allow to add, remove, search elements for o(log(n)) 
    /// </summary>
    type Tree<'T when 'T : comparison>() =
        /// Head of the tree
        let mutable head = TreeElement.Null
        /// Count of elements in tree
        let mutable count = 0
        
        /// IEnumerable interface that gets enumerator
        interface IEnumerable<'T> with
            /// <summary>
            /// Gets generic enumerator
            /// </summary>
            member this.GetEnumerator(): IEnumerator<'T> = 
                let seq = this.GetSeq()
                seq.GetEnumerator()
            
            /// <summary>
            /// Gets ordinary enumerator
            /// </summary>
            member this.GetEnumerator(): System.Collections.IEnumerator = 
                let seq = this.GetSeq()
                seq.GetEnumerator() :> IEnumerator            
        
        /// <summary>
        /// Gets count of elements
        /// </summary>
        member this.Count 
            with get() = count
        
        /// <summary>
        /// To seq
        /// </summary>
        member this.GetSeq() = 
            let rec getValues head = seq {
                match head with
                | Null -> yield! Seq.empty
                | TreeElement(value, left, right) -> yield value 
                                                     yield! getValues left
                                                     yield! getValues right
            }
            getValues head
        
        /// <summary>
        /// Adds value to collection
        /// </summary>
        /// <param name="value">Value to add</param>
        member this.Add value = 
            let rec add head value = 
                match head with
                | TreeElement(headValue, left, right) -> if value = headValue then TreeElement(headValue, left, right) 
                                                         elif value < headValue then TreeElement(headValue, add left value, right)
                                                         else TreeElement(headValue, left, add right value)
                | Null -> count <- count + 1 
                          TreeElement(value, Null, Null)
            head <- add head value
        
        /// <summary>
        /// Checks containment of value
        /// </summary>
        /// <param name="value">Value to check</param>
        member this.IsContains value = 
            let rec isContains head value =
                match head with
                | Null -> false
                | TreeElement(headValue, left, right) -> if value = headValue then true
                                                         elif value < headValue then isContains left value
                                                         else isContains right value
            isContains head value
        
        /// <summary>
        /// Removes value from collection
        /// </summary>
        /// <param name="value">Value to remove</param>
        /// <exception cref="ArgumentException">Throws when value does not exist</exception>
        member this.Remove value =  
            let rec getRightTip head =
                match head with
                | Null -> raise (ArgumentException("head is null"))
                | TreeElement(value, Null, Null) -> value
                | TreeElement(_, _, right) -> getRightTip right
            let rec remove head value = 
                match head with
                | Null -> raise (ArgumentException("element is not found"))
                | TreeElement (headValue, left, right) as x -> 
                    if value < headValue then TreeElement(headValue, remove left value, right)
                    elif value > headValue then TreeElement(headValue, left, remove right value)
                    else 
                        match head with 
                        | TreeElement(_, Null, Null) -> Null
                        | TreeElement(_, left, Null) -> left
                        | TreeElement(_, Null, right) -> right
                        | TreeElement(_, left, right) -> let tip = getRightTip left 
                                                         TreeElement(tip, remove left tip, right)
                        | Null -> failwith "head is null"
            count <- count - 1
            head <- remove head value

    open Robots
                
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        let tree = Tree<int>()
        tree.Add(5)
        tree.Add(6)
        for i in [1..30] do tree.Add(i)
        tree.Add(7)
        tree.Remove(7)
        tree.IsContains(7) |> printfn "%A"
        tree.IsContains(5) |> printfn "%A"
        tree.GetSeq() |> Seq.toList |> List.sort |> printfn "%A"
        let mutable seq = Seq.empty
        for i in tree do seq <- Seq.append [i] seq
        seq |> Seq.toList |> List.sort |> printfn "%A"
        do Console.WriteLine()
        let comps = [Machine("Linux", 0.4, true); Machine("Mac", 0.2, false); Machine("Windows", 0.8, false)]
        let matrix = [[true; false; true]
                      [false; true; true]
                      [true; true; true]]
        let net = new Net(comps, matrix)
        for i in [1 .. 10] do
            net.NewTurn()
            printfn "%A" net.InfectedComputers
        net.IsEndOfProcess() |> printfn "%A"
        0 // return an integer exit code
