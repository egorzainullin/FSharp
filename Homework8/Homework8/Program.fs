// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Text.RegularExpressions
open System
open System.IO
open System.Net

let link = @"../../example.html"

let text = Array.fold (fun state i -> state + "/n" + i) "" (System.IO.File.ReadAllLines(link)) 

let getAllHrefs htmlText = 
    let pattern = "<a\\shref\\s*=\\s*\"http://(?:(?<1>[^\"']*)\"|(?<1>\\S+))"
    let matched = Regex.Match(htmlText, pattern, RegexOptions.IgnoreCase, TimeSpan.FromSeconds(0.5))
    let rec getListOfMatched (matched : Match) list =
        if matched.Success then let link = "http://" + matched.Groups.[1].Value
                                getListOfMatched (matched.NextMatch()) (link :: list)
        else list
    getListOfMatched matched []

let loadPagesAndPrintNumberParallel (url : string) =
    let printNumberOfSymbolsAsync (page : string) = 
        async {
            let request = WebRequest.Create(page)
            use! response = request.AsyncGetResponse()
            use stream = response.GetResponseStream()
            use reader = new StreamReader(stream)
            let html = reader.ReadToEnd()
            return (page, html.Length)
        }
    let request = WebRequest.Create(url)
    let response = request.GetResponse()
    use stream = response.GetResponseStream()
    use reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    let pages = getAllHrefs html
    pages |> (List.map printNumberOfSymbolsAsync) |> Async.Parallel |> Async.RunSynchronously
       
getAllHrefs text |> printfn "%A"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    loadPagesAndPrintNumberParallel @"http://hwproj.me/courses/9/terms/4" |> printfn "%A"
    0 // return an integer exit code
