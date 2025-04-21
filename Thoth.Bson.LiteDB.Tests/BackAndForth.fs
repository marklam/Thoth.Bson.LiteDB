module Tests.BackAndForth

open Thoth.Json.Net
open Util.Testing
open System
open Hedgehog
open Tests.Types

type TestRecord =
    {
        Name : string option
        Entries : int list
    }
    with
        static member Decode : Decoder<TestRecord> =
            Decode.object (fun get ->
                { Name = get.Required.Field "name" (Decode.option Decode.string)
                  Entries = get.Required.Field "entries" (Decode.list Decode.int) })

        static member Encode (value : TestRecord) =
            Encode.object [
                "name", value.Name |> Encode.option Encode.string
                "entries", value.Entries |> List.map Encode.int |> Encode.list
            ]

type TestDU =
    | Nowt
    | String of string
    | Char of char
    | Int of int
    | Float of float
    | Single of single
    | Decimal of decimal
    | Map of Map<Guid, TestRecord>
    | BigInt of bigint
    | DateTime of DateTime
    | DateTimeOffset of DateTimeOffset
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Tuple of (string * int * float)
    | Bool of bool
    | Guid of Guid
    | List of TestRecord list
    with
        static member Decode : Decoder<TestDU> =
            Decode.index 0 Decode.string
            |> Decode.andThen (fun typ ->
                match typ with
                | "Nowt"           -> Decode.succeed Nowt
                | "String"         -> Decode.map String Decode.string
                | "Char"           -> Decode.map Char Decode.char
                | "Int"            -> Decode.map Int Decode.int
                | "Float"          -> Decode.map Float Decode.float
                | "Single"         -> Decode.map Single Decode.float32
                | "Decimal"        -> Decode.map Decimal Decode.decimal
                | "Map"            -> Decode.map Map (Decode.map' Decode.guid TestRecord.Decode)
                | "BigInt"         -> Decode.map BigInt Decode.bigint
                | "DateTime"       -> Decode.map DateTime Decode.datetimeUtc
                | "DateTimeOffset" -> Decode.map DateTimeOffset Decode.datetimeOffset
                | "SByte"          -> Decode.map SByte Decode.sbyte
                | "Byte"           -> Decode.map Byte Decode.byte
                | "Int16"          -> Decode.map Int16 Decode.int16
                | "UInt16"         -> Decode.map UInt16 Decode.uint16
                | "UInt32"         -> Decode.map UInt32 Decode.uint32
                | "Int64"          -> Decode.map Int64 Decode.int64
                | "UInt64"         -> Decode.map UInt64 Decode.uint64
                | "Bool"           -> Decode.map Bool Decode.bool
                | "Guid"           -> Decode.map Guid Decode.guid
                | "Tuple"          -> Decode.map Tuple (Decode.tuple3 Decode.string Decode.int Decode.float)
                | "List"           -> Decode.map List (Decode.list TestRecord.Decode)
                | unknown          -> Decode.fail (sprintf "Unknown type: %s" unknown)
            )

        static member Encode (value : TestDU) =
            match value with
            | Nowt               -> Encode.list [ Encode.string "Nowt" ]
            | String str         -> Encode.list [ Encode.string "String"; Encode.string str ]
            | Char c             -> Encode.list [ Encode.string "Char"; Encode.char c ]
            | Int i              -> Encode.list [ Encode.string "Int"; Encode.int i ]
            | Float f            -> Encode.list [ Encode.string "Float"; Encode.float f ]
            | Single s           -> Encode.list [ Encode.string "Single"; Encode.float32 s ]
            | Decimal d          -> Encode.list [ Encode.string "Decimal"; Encode.decimal d ]
            | Map m              -> Encode.list [ Encode.string "Map"; Encode.map Encode.guid TestRecord.Encode m ]
            | BigInt b           -> Encode.list [ Encode.string "BigInt"; Encode.bigint b ]
            | DateTime dt        -> Encode.list [ Encode.string "DateTime"; Encode.datetime dt ]
            | DateTimeOffset dto -> Encode.list [ Encode.string "DateTimeOffset"; Encode.datetimeOffset dto ]
            | SByte sb           -> Encode.list [ Encode.string "SByte"; Encode.sbyte sb ]
            | Byte b             -> Encode.list [ Encode.string "Byte"; Encode.byte b ]
            | Int16 i            -> Encode.list [ Encode.string "Int16"; Encode.int16 i ]
            | UInt16 i           -> Encode.list [ Encode.string "UInt16"; Encode.uint16 i ]
            | UInt32 i           -> Encode.list [ Encode.string "UInt32"; Encode.uint32 i ]
            | Int64 i            -> Encode.list [ Encode.string "Int64"; Encode.int64 i ]
            | UInt64 i           -> Encode.list [ Encode.string "UInt64"; Encode.uint64 i ]
            | Bool b             -> Encode.list [ Encode.string "Bool"; Encode.bool b ]
            | Guid g             -> Encode.list [ Encode.string "Guid"; Encode.guid g ]
            | Tuple (s, i, f)    -> Encode.list [ Encode.string "Tuple"; Encode.tuple3 Encode.string Encode.int Encode.float (s, i, f) ]
            | List lst           -> Encode.list [ Encode.string "List"; lst |> List.map TestRecord.Encode |> Encode.list ]

let tests : Test =

    testList "Thoth.Json - Back and Forth" [

        testList "Roundtrip" [
            testCase "TestRecord" <| fun _ ->
                property {
                    let! expected = GenX.auto<TestRecord>
                    let bson = TestRecord.Encode expected
                    let actual = TestRecord.Decode "" bson
                    equal (Ok expected) actual
                } |> Property.check

            testCase "TestDU" <| fun _ ->
                property {
                    let! expected = GenX.auto<TestDU>
                    let bson = TestDU.Encode expected
                    let actual = TestDU.Decode "" bson
                    equal (Ok expected) actual
                } |> Property.check
        ]
    ]
