// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec productOfDigits x =
    let n = abs x
    match n with
    | d when d >=0 && d <=9 -> d  
    | _ -> (productOfDigits (n / 10)) * (n % 10)

let product = productOfDigits 241

let firstAppearanceInList list n =
    let rec firstAppearanceInList' list n counter =
        match list with
        | h :: tail when h = n -> Some(counter + 1)
        | h :: tail -> firstAppearanceInList' tail n (counter + 1)
        | _ -> None
    firstAppearanceInList' list n 0

let count = firstAppearanceInList [1; 2; 3; 4] 2

let rec isPalindrom (str:string) = 
    let length = str.Length
    if (length <= 1) then true 
    elif str.[0] = str.[length - 1] then isPalindrom str.[1 .. length - 2]
    else false

let palindrom = isPalindrom "abba" 

let rec mergesort (list : 'T list) = 
    let rec reverse list result = 
        match list with
        | h :: tail -> reverse list.Tail (h :: result)
        | _ -> result
    let rec merge (list1 : 'T list) (list2 : 'T list) result = 
        if list1.IsEmpty then reverse ((reverse list2 []) @ result) []
        elif list2.IsEmpty then reverse ((reverse list1 []) @ result) []
        elif list1.Head <= list2.Head then merge list1.Tail list2 (list1.Head :: result)
        else merge list1 list2.Tail (list2.Head :: result)
    if list.Length <= 1 then list
    else 
        let length = list.Length
        let list1 = mergesort list.[0 .. (length - 1)/ 2]
        let list2 = mergesort list.[((length - 1) / 2 + 1) .. (length - 1)]
        merge list1 list2 []

let merged = mergesort [6; 5; 2; 3; 4; 1]

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
