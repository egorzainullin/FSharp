namespace Hw7Tests

module Tests =
    
    open FsUnit
    open NUnit.Framework

    open Program

    [<Test>]
    let ``string converter is OK 1``() =
        let converter = new StringConvertBuilder()
        let z = converter { 
            let! x = "1"
            let! y = "2"        
            let z = x + y
            return z }
        z |> should equal (Some(3))

    [<Test>]
    let ``string converter is OK 2``() =
        let converter = new StringConvertBuilder()
        let z = converter { 
            let! x = "1"
            let! y = "s2"        
            let z = x + y
            return z }
        z |> should equal None

    [<Test>]
    let ``precise computation is OK 1``() =
        let n = preciseCalculation 3 {
                let! a = 2.0 / 12.0
                let! b = 3.5
                return a / b
                }
        n |> should equal 0.048
    
    [<Test>]
    let ``let precise computation is OK 2``() =
        let n = preciseCalculation 2 {
               let! a = 2.0 / 12.0
               let! b = 3.5
               return a / b
               }
        n |> should equal 0.05
