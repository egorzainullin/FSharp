// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

namespace Hw5

module PointFree =

    let f1 x l = List.map ((*) x) l

    let f2 x = List.map ((*) x)
    
    let f3 = List.map << (*)     

    open Brackets

    open Telephone

    [<EntryPoint>]
    let main argv =
        let l = [("89", "Alex"); ("89", "Susan"); ("56", "Max")]
        interactive l |> ignore
        0 // return an integer exit code
