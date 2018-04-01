// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

namespace Hw5

module PointFree =

    let f1 x l = List.map ((*) x) l

    let f2 x = List.map ((*) x)
    
    let f3 = List.map << (*)
    
    do f3 2 [1; 2; 3] |> printfn "%A" 

    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
