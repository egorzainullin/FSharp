namespace Homework6

module BinaryTree = 
    
    type TreeElement<'T> = 
        | Null
        | TreeElement of 'T * TreeElement<'T> * TreeElement<'T>

    type Tree<'T>(initSeq) =
        let mutable head = TreeElement.Null
        let mutable count = 0
        member this.Head 
            with get() = head;
        member this.Count 
            with get() = count
        member this.Add value = 
            let rec add head value = 
                match head with
                | TreeElement(headValue, left, right) -> if value = headValue then TreeElement(headValue, left, right) 
                                                         elif value < headValue then TreeElement(headValue, add left value, right)
                                                         else TreeElement(headValue, left, add right value)
                | Null -> count <- count + 1 
                          TreeElement(value, Null, Null)
            head <- add head value

    let tree = Tree<int>()
    tree.Add(5)
    tree.Add(6)
    tree.Head |> printfn "%A"

    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
