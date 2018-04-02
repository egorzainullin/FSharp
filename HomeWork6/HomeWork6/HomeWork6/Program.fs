namespace Homework6

module BinaryTree = 
    open System.Collections.Generic
    open System.Collections
    open System
    
    type internal TreeElement<'T> = 
        | Null
        | TreeElement of 'T * TreeElement<'T> * TreeElement<'T>

    type Tree<'T when 'T : comparison>(initSeq) =
        let mutable head = TreeElement.Null
        let mutable count = 0

        interface IEnumerable<'T> with
            member this.GetEnumerator(): IEnumerator<'T> = 
                let seq = this.GetSeq
                seq.GetEnumerator()
            
            member this.GetEnumerator(): System.Collections.IEnumerator = 
                let seq = this.GetSeq
                seq.GetEnumerator() :> IEnumerator            
        
        member this.Count 
            with get() = count
        
        member this.GetSeq = 
            let rec getValues head = seq {
                match head with
                | Null -> yield! Seq.empty
                | TreeElement(value, left, right) -> yield value 
                                                     yield! getValues left
                                                     yield! getValues right
            }
            getValues head

        member this.Add value = 
            let rec add head value = 
                match head with
                | TreeElement(headValue, left, right) -> if value = headValue then TreeElement(headValue, left, right) 
                                                         elif value < headValue then TreeElement(headValue, add left value, right)
                                                         else TreeElement(headValue, left, add right value)
                | Null -> count <- count + 1 
                          TreeElement(value, Null, Null)
            head <- add head value
        
        member this.IsContains value = 
            let rec isContains head value =
                match head with
                | Null -> false
                | TreeElement(headValue, left, right) -> if value = headValue then true
                                                         elif value < headValue then isContains left value
                                                         else isContains right value
            isContains head value
        
        member this.Remove value =  
            let rec getRightTip head =
                match head with
                | Null -> failwith "head is null"
                | TreeElement(value, Null, Null) -> value
                | TreeElement(_, _, right) -> getRightTip right
            let rec remove head value = 
                match head with
                | Null -> failwith "element is not found"
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
            head <- remove head value

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
        tree.GetSeq |> Seq.toList |> List.sort |> printfn "%A"
        let mutable seq = Seq.empty
        for i in tree do seq <- Seq.append [i] seq
        seq |> Seq.toList |> List.sort |> printfn "%A"
        do Console.WriteLine()
        0 // return an integer exit code
