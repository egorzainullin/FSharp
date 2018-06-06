namespace Hw8Tests

module Tests =
    open FsUnit
    open NUnit.Framework
    open Program
    
    [<Test>]
    let ``check that finding urls is ok``() = 
        let link = @"Hw8Tests/example.html"
        let text = Array.fold (fun state i -> state + "/n" + i) "" (System.IO.File.ReadAllLines(link))    
        getAllHrefs text |> List.item 1 |> should equal "http://yandex.ru"
        getAllHrefs text |> List.item 0 |> should equal "http://google.com"