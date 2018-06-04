namespace Hw5

module Telephone =
    
    open System

    let addToBase phone name list = (phone, name) :: list

    let findNameByPhone phone list =
        list |> List.filter (fun (current, _) -> current = phone)
        |> List.map (snd)

    let findPhoneByName name list =
        list |> List.filter (fun (_, current) -> current = name)
        |> List.map (fst)

    let rec printData list = 
        match list with
            | [] -> printfn "%s" "_"
            | h :: tail -> printfn "%A" h
                           printData tail

    open System.IO
    open System.Runtime.Serialization.Formatters.Binary

    let saveToFile list =
        let formatter = new BinaryFormatter()
        use stream = new FileStream("output.dat", FileMode.Create)
        formatter.Serialize(stream, box list)

    let readFromFile fileName =
        let formatter = new BinaryFormatter()
        try
            use stream = new FileStream(fileName, FileMode.Open)
            let dat = formatter.Deserialize(stream)
            match dat with
            | :? ((string * string) list) as l -> l 
            | _ -> raise (ArgumentException("Incorrect data"))
                                    
        with
            | :? ArgumentException ->  raise (ArgumentException("Can't read file, filename is incorrect"))
            | :? FileNotFoundException -> raise (ArgumentException("Can't find file with this name"))

    let printAllInfo = 
        printfn "%s" "Type 1 to exit"
        printfn "%s" "Type 2 to add new contact"
        printfn "%s" "Type 3 to find name by phone"
        printfn "%s" "Type 4 to find phone by name"
        printfn "%s" "Type 5 to print all"
        printfn "%s" "Type 6 to save into file"
        printfn "%s" "Type 7 to read from file"

    let rec interactive list =
        printAllInfo
        let command = Console.ReadLine()
        match command with
        | "1" -> ignore
        | "2" -> printfn "%s" "type phone & name"
                 let phone = Console.ReadLine()
                 let name = Console.ReadLine()
                 printfn "%s" "adding.."
                 interactive (addToBase phone name list)
        | "3" -> printfn "%s" "type phone"
                 let phone = Console.ReadLine()
                 printfn "%A" (findNameByPhone phone list)
                 interactive list
        | "4" -> printfn "%s" "type name"
                 let name = Console.ReadLine()
                 printfn "%A" (findPhoneByName name list)
                 interactive list
        | "5" -> printData list
                 interactive list
        | "6" -> saveToFile list
                 printfn "%s" "saving..."
                 interactive list
        | "7" -> printfn "%s" "type file name to load"
                 let fileName = Console.ReadLine()
                 try 
                    let fileData = readFromFile fileName
                    printfn "%s" "successful"
                    interactive fileData
                 with 
                 | :? ArgumentException as e -> printfn "%s" e.Message
                                                interactive list
        | _ -> printfn "%s" "Incorrect command try again"
               interactive list