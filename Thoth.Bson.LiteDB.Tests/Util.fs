module Util.Testing

open System
open Expecto

let testList name tests = testList name tests
let testCase msg test = testCase msg test
let ftestCase msg test = ftestCase msg test

let inline equal expected actual: unit =
    Expect.equal actual expected ""

let inline equalMultiline expected actual =
    let fixNL (str: string) =
        str.Split([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.TrimEnd())
        |> String.concat Environment.NewLine
        |> fun s -> s.TrimEnd([|'\n';'\r'|])

    Expect.equal (fixNL actual) (fixNL expected) ""

let inline equalResult expected actual: unit =
    match expected, actual with
    | Error expected, Error actual ->
        equalMultiline expected actual
    | _,_ ->
        equal expected actual

type Test = Expecto.Test
