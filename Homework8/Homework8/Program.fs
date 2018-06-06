// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Text.RegularExpressions
open System
open System.IO
open System.Net

let getAllHrefs htmlText = 
    let pattern = "<a\\shref\\s*=\\s*\"http://(?:(?<1>[^\"']*)\"|(?<1>\\S+))"
    let matched = Regex.Match(htmlText, pattern, RegexOptions.IgnoreCase, TimeSpan.FromSeconds(0.5))
    let rec getListOfMatched (matched : Match) list =
        if matched.Success then let link = "http://" + matched.Groups.[1].Value
                                getListOfMatched (matched.NextMatch()) (link :: list)
        else list
    getListOfMatched matched []

let loadPagesAndPrintNumberParallel (url : string) =
    let getHtml (response : WebResponse) =
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        reader.ReadToEnd()
    let printNumberOfSymbolsAsync (page : string) = 
        async {
            try 
                let request = WebRequest.Create(page)
                use! response = request.AsyncGetResponse() 
                let html = getHtml response    
                return Some((page, html.Length))
            with 
            | e -> return None
        }
    let request = WebRequest.Create(url)
    let response = request.GetResponse()
    let html = getHtml response 
    let pages = getAllHrefs html
    pages |> (List.map printNumberOfSymbolsAsync) |> Async.Parallel |> Async.RunSynchronously |> Array.toList
    |> List.choose id

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    loadPagesAndPrintNumberParallel @"http://hwproj.me/courses/9/terms/4" |> printfn "%A"
    0 // return an integer exit code
