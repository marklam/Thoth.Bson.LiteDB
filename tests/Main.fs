module Tests.Main

open Expecto
open Util.Testing

[<Tests>]
let tests =
    testList "All" [
        Decoders.tests
        Encoders.tests
        BackAndForth.tests
        ExtraCoders.tests
    ]

[<EntryPoint>]
let main args =
    tests
    |> runTestsWithCLIArgs [] args
