namespace HwTests

module FakeRandom =
    
    open System

    type FakeRandom(values) =
        inherit Random()

        let mutable counter = 0

        let plusOne() = 
            let temp = counter
            counter <- counter + 1
            temp

        new() = FakeRandom([|0.5|])

        override this.NextDouble() = values.[(plusOne()) % values.Length]