namespace HwTests

module NetTests =

    open NUnit.Framework
    open FsUnit

    // Note: Linux probability - 0.4, Mac - 0.2 , Windows - 0.8

    open Homework6
    open Net
    open FakeRandom

    [<Test>]
    let ``test factory is OK``() =
        let linux = Factory.CreateLinux true
        linux.TypeOfOs |> should equal "Linux"
        linux.Possibility |> should equal 0.4
        linux.IsInfected |> should equal true
        let windows = Factory.CreateWindows false
        windows.TypeOfOs |> should equal "Windows"
        windows.Possibility |> should equal 0.8
        windows.IsInfected |> should equal false
        let mac = Factory.CreateMac true
        mac.TypeOfOs |> should equal "Mac"

    let mutable net = Net([], [])

    let mutable matrix = []

    let mutable computers = []

    [<SetUp>]
    let ``init tests before execution``() =
        matrix <- [ [true; false; true];
                    [false; true; true];
                    [true; true; true ] ]
        computers <- [Factory.CreateLinux true; Factory.CreateMac false; Factory.CreateWindows false]
        let rand = FakeRandom()
        net <- Net(computers, matrix, rand);

    [<Test>]
    let ``test new turn 1``() =
        net.NewTurn()
        let comps = net.InfectedComputers
        comps |> should equal [true; false; true]
        net.NewTurn()
        comps.[1] |> should equal false

    [<Test>]
    let ``test new turn 2``() =
        matrix <- [ [ true; true; false];
                  [ false; true; false ];
                  [ false; false; true ] ]
        let rand = FakeRandom([| 0.2 |])
        net <- Net(computers, matrix, rand)
        net.NewTurn()
        let comps = net.InfectedComputers
        comps |> should equal [true; true; false]
        net.NewTurn()
        comps.[2] |> should equal false

    [<Test>]
    let ``is end of process test 1``() = 
        let rand = FakeRandom([| 0.2 |])
        let net = Net(computers, matrix, rand)
        net.NewTurn()
        net.IsEndOfProcess() |> should equal false
        net.NewTurn()
        net.IsEndOfProcess() |> should equal true

    [<Test>]
    let ``is end of process test 2``() =
        let rand = FakeRandom([| 0.2 |])
        let comp = Machine("InvincibleOS", 0.0, false)
        computers <- [comp]
        matrix <- [[true]]
        let net = Net(computers, matrix, rand)
        net.IsEndOfProcess() |> should equal true
