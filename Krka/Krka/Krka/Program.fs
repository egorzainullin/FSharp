module Krka1

open System
open System.Security.Cryptography.X509Certificates

let findAverageSin list =
    let rec findAverageSin' list sum =
        match list with
        | h :: t -> findAverageSin' t (sum + (sin h))
        | _ -> sum
    (findAverageSin' list 0.0) / ((float) list.Length)

type Tree<'T> = 
    | Null
    | Tree of 'T * (Tree<'T> list)

let findShortest tree =
    let rec findShortest' distance tree = 
        match tree with
        | Tree(_, list) when List.length list <> 0-> let min = list |> List.map (findShortest' 0) |> List.min in (distance + min + 1)
        | _ -> distance
    if (tree <> Null) then (findShortest' 0 tree) else failwith "Tree is null"

type internal QueueElement<'T> =
    | Nullptr
    | QueueElement of 'T * QueueElement<'T>

type Queue<'T>() =
    let mutable head = Nullptr

    member this.Enqueue x =
        match head with
        | Nullptr -> head <- QueueElement(x, Nullptr)
        | _ -> let newElement = QueueElement(x, head)
               head <- newElement

    member this.Dequeue () =
        let rec findLastAndDelete head =
            match head with
            | Nullptr -> failwith "queue is empty"
            | QueueElement(x, next) when next<> Nullptr -> let temp = findLastAndDelete next in (QueueElement(x, fst temp), snd temp)
            | QueueElement(x, _) -> (Nullptr, x)
        match head with
        | Nullptr -> failwith "queue is empty"
        | QueueElement(x, Nullptr) -> let z = x 
                                      head <- Nullptr
                                      z
        | _ -> (findLastAndDelete head) |> snd

let queue = Queue<int>()

queue.Enqueue(5)
queue.Enqueue(7)

printfn "%A" (queue.Dequeue())
printfn "%A" (queue.Dequeue())

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
