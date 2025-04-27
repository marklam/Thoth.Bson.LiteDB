module Tests.Encoders

open Thoth.Json.Net
open Util.Testing
open System
open VerifyExpecto
open VerifyTests
open Expecto
open Argon
open Tests.Types


type RecordWithPrivateConstructor = private { Foo1: int; Foo2: float }
type UnionWithPrivateConstructor = private Bar of string | Baz

let tests : Test =
    VerifierSettings.AddExtraSettings(fun settings -> settings.AddFSharpConverters())

    testList "Thoth.Json.Encode" [

        testList "Basic" [

            testCase "a string works" <| fun _ ->
                let expected = "\"maxime\""
                let actual =
                    Encode.string "maxime"
                    |> Encode.toString 0
                equal expected actual

            testCase "a string with new line works" <| fun _ ->
                let expected = "\"a\\nb\""
                let actual =
                    Encode.string "a\nb"
                    |> Encode.toString 4

                equal expected actual

            testCase "a string with new line character works" <| fun _ ->
                let expected = "\"a\\\\nb\""
                let actual =
                    Encode.string "a\\nb"
                    |> Encode.toString 4

                equal expected actual

            testCase "a string with tab works" <| fun _ ->
                let expected = "\"a\\tb\""
                let actual =
                    Encode.string "a\tb"
                    |> Encode.toString 4

                equal expected actual

            testCase "a string with tab character works" <| fun _ ->
                let expected = "\"a\\\\tb\""
                let actual =
                    Encode.string "a\\tb"
                    |> Encode.toString 4

                equal expected actual

            testCase "a char works" <| fun _ ->
                let expected = "\"a\""
                let actual =
                    Encode.char 'a'
                    |> Encode.toString 0

                equal expected actual

            testCase "an int works" <| fun _ ->
                let expected = "1"
                let actual =
                    Encode.int 1
                    |> Encode.toString 0
                equal expected actual

            testCase "a float works" <| fun _ ->
                let expected = "1.2"
                let actual =
                    Encode.float 1.2
                    |> Encode.toString 0
                equal expected actual

            testCase "an array works" <| fun _ ->
                let expected =
                    """["maxime",2]"""
                let actual =
                    Encode.array
                        [| Encode.string "maxime"
                           Encode.int 2
                        |] |> Encode.toString 0
                equal expected actual

            testCase "a list works" <| fun _ ->
                let expected =
                    """["maxime",2]"""
                let actual =
                    Encode.list
                        [ Encode.string "maxime"
                          Encode.int 2
                        ] |> Encode.toString 0
                equal expected actual

            testCase "a bool works" <| fun _ ->
                let expected = "false"
                let actual =
                    Encode.bool false
                    |> Encode.toString 0
                equal expected actual

            testCase "a null works" <| fun _ ->
                let expected = "null"
                let actual =
                    Encode.nil
                    |> Encode.toString 0
                equal expected actual

            testCase "unit works" <| fun _ ->
                let expected = "null"
                let actual =
                    Encode.unit ()
                    |> Encode.toString 0
                equal expected actual

            testCase "an object works" <| fun _ ->
                let expected =
                    """{"firstname":"maxime","age":25}"""
                let actual =
                    Encode.object
                        [ ("firstname", Encode.string "maxime")
                          ("age", Encode.int 25)
                        ] |> Encode.toString 0
                equal expected actual

            testCase "a dict works" <| fun _ ->
                let expected =
                    """{"a":1,"b":2,"c":3}"""
                let actual =
                    Map.ofList
                        [ ("a", Encode.int 1)
                          ("b", Encode.int 2)
                          ("c", Encode.int 3)
                        ]
                    |> Encode.dict
                    |> Encode.toString 0
                equal expected actual

            testCase "a map works" <| fun _ ->
                let expected =
                    """[["a",1],["b",2],["c",3]]"""
                let actual =
                    Map.ofList
                        [ ("a", 1)
                          ("b", 2)
                          ("c", 3)
                        ]
                    |> Encode.map Encode.string Encode.int
                    |> Encode.toString 0
                equal expected actual

            testCase "a bigint works" <| fun _ ->
                let expected = "\"12\""
                let actual =
                    Encode.bigint 12I
                    |> Encode.toString 0

                equal expected actual

            testCase "a datetime works" <| fun _ ->
                let expected = "{\"$date\":\"2018-10-01T11:12:55.0000000Z\"}"
                let actual =
                    DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc)
                    |> Encode.datetimeUtc
                    |> Encode.toString 0

                equal expected actual

            testCase "a datetime with offset works (only in UTC+1)" <| fun _ ->
                let expected = "{\"$date\":\"2018-10-01T10:12:55.0000000Z\"}"
                let actual =
                    DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Local)
                    |> Encode.datetimeUtc
                    |> Encode.toString 0

                equal expected actual

            testCase "a datetimeOffset works" <| fun _ ->
                let expected = "\"2018-07-02T12:23:45.0000000+02:00\""
                let actual =
                    DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
                    |> Encode.datetimeOffset
                    |> Encode.toString 0

                equal expected actual

            testCase "a timeSpan works" <| fun _ ->
                let expected = "\"1.02:03:04.0050000\""
                let actual =
                    TimeSpan(1, 2, 3, 4, 5)
                    |> Encode.timespan
                    |> Encode.toString 0

                equal expected actual

            testCase "a decimal works" <| fun _ ->
                let expected = "{\"$numberDecimal\":\"0.7833\"}"
                let actual =
                    0.7833M
                    |> Encode.decimal
                    |> Encode.toString 0

                equal expected actual

            testCase "a guid works" <| fun _ ->
                let expected = "{\"$guid\":\"1e5dee25-8558-4392-a9fb-aae03f81068f\"}"
                let actual =
                    Guid.Parse("1e5dee25-8558-4392-a9fb-aae03f81068f")
                    |> Encode.guid
                    |> Encode.toString 0

                equal expected actual

            testCase "an byte works" <| fun _ ->
                let expected = "99"
                let actual =
                    99uy
                    |> Encode.byte
                    |> Encode.toString 0

                equal expected actual

            testCase "an sbyte works" <| fun _ ->
                let expected = "99"
                let actual =
                    99y
                    |> Encode.sbyte
                    |> Encode.toString 0

                equal expected actual

            testCase "an int16 works" <| fun _ ->
                let expected = "99"
                let actual =
                    99s
                    |> Encode.int16
                    |> Encode.toString 0

                equal expected actual

            testCase "an uint16 works" <| fun _ ->
                let expected = "99"
                let actual =
                    99us
                    |> Encode.uint16
                    |> Encode.toString 0

                equal expected actual

            testCase "an int64 works" <| fun _ ->
                let expected = "{\"$numberLong\":\"7923209\"}"
                let actual =
                    7923209L
                    |> Encode.int64
                    |> Encode.toString 0

                equal expected actual

            testCase "an uint64 works" <| fun _ ->
                let expected = "{\"$numberDecimal\":\"7923209\"}"
                let actual =
                    7923209UL
                    |> Encode.uint64
                    |> Encode.toString 0

                equal expected actual

            testCase "an enum<sbyte> works" <| fun _ ->
                let expected = "99"
                let actual =
                    Encode.toString 0 (Encode.Enum.sbyte Enum_Int8.NinetyNine)

                equal expected actual

            testCase "an enum<byte> works" <| fun _ ->
                let expected = "99"
                let actual =
                    Encode.toString 0 (Encode.Enum.byte Enum_UInt8.NinetyNine)

                equal expected actual

            testCase "an enum<int> works" <| fun _ ->
                let expected = "1"
                let actual =
                    Encode.toString 0 (Encode.Enum.int Enum_Int.One)

                equal expected actual

            testCase "an enum<uint32> works" <| fun _ ->
                let expected = "{\"$numberLong\":\"99\"}"
                let actual =
                    Encode.toString 0 (Encode.Enum.uint32 Enum_UInt32.NinetyNine)

                equal expected actual

            testCase "an enum<int16> works" <| fun _ ->
                let expected = "99"
                let actual =
                    Encode.toString 0 (Encode.Enum.int16 Enum_Int16.NinetyNine)

                equal expected actual

            testCase "an enum<uint16> works" <| fun _ ->
                let expected = "99"
                let actual =
                    Encode.toString 0 (Encode.Enum.uint16 Enum_UInt16.NinetyNine)

                equal expected actual

            testCase "a tuple2 works" <| fun _ ->
                let expected = """[1,"maxime"]"""
                let actual =
                    Encode.tuple2
                        Encode.int
                        Encode.string
                        (1, "maxime")
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple3 works" <| fun _ ->
                let expected = """[1,"maxime",2.5]"""
                let actual =
                    Encode.tuple3
                        Encode.int
                        Encode.string
                        Encode.float
                        (1, "maxime", 2.5)
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple4 works" <| fun _ ->
                let expected = """[1,"maxime",2.5,{"fieldA":"test"}]"""
                let actual =
                    Encode.tuple4
                        Encode.int
                        Encode.string
                        Encode.float
                        SmallRecord.Encoder
                        (1, "maxime", 2.5, { fieldA = "test" })
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple5 works" <| fun _ ->
                let expected = """[1,"maxime",2.5,{"fieldA":"test"},{"$date":"2018-10-01T11:12:55.0000000Z"}]"""
                let actual =
                    Encode.tuple5
                        Encode.int
                        Encode.string
                        Encode.float
                        SmallRecord.Encoder
                        Encode.datetimeUtc
                        (1, "maxime", 2.5, { fieldA = "test" }, DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc))
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple6 works" <| fun _ ->
                let expected = """[1,"maxime",2.5,{"fieldA":"test"},false,null]"""
                let actual =
                    Encode.tuple6
                        Encode.int
                        Encode.string
                        Encode.float
                        SmallRecord.Encoder
                        Encode.bool
                        (fun _ -> Encode.nil)
                        (1, "maxime", 2.5, { fieldA = "test" }, false, null)
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple7 works" <| fun _ ->
                let expected = """[1,"maxime",2.5,{"fieldA":"test"},false,null,true]"""
                let actual =
                    Encode.tuple7
                        Encode.int
                        Encode.string
                        Encode.float
                        SmallRecord.Encoder
                        Encode.bool
                        (fun _ -> Encode.nil)
                        Encode.bool
                        (1, "maxime", 2.5, { fieldA = "test" }, false, null, true)
                    |> Encode.toString 0

                equal expected actual

            testCase "a tuple8 works" <| fun _ ->
                let expected = """[1,"maxime",2.5,{"fieldA":"test"},false,null,true,98]"""
                let actual =
                    Encode.tuple8
                        Encode.int
                        Encode.string
                        Encode.float
                        SmallRecord.Encoder
                        Encode.bool
                        (fun _ -> Encode.nil)
                        Encode.bool
                        Encode.int
                        (1, "maxime", 2.5, { fieldA = "test" }, false, null, true, 98)
                    |> Encode.toString 0

                equal expected actual

            testTask "using pretty space works" {
                let actual =
                    Encode.object
                        [ ("firstname", Encode.string "maxime")
                          ("age", Encode.int 25)
                        ] |> Encode.toString 4
                do! Verifier.Verify("using pretty space works", actual).ToTask()
            }

            testTask "complex structure works" {
                let actual =
                    Encode.object
                        [ ("firstname", Encode.string "maxime")
                          ("age", Encode.int 25)
                          ("address", Encode.object
                                        [ "street", Encode.string "main road"
                                          "city", Encode.string "Bordeaux"
                                        ])
                        ] |> Encode.toString 4
                do! Verifier.Verify("complex structure works", actual).ToTask()
            }

            testCase "option with a value `Some ...` works" <| fun _ ->
                let expected = """{"id":1,"operator":"maxime"}"""

                let actual =
                    Encode.object
                        [ ("id", Encode.int 1)
                          ("operator", Encode.option Encode.string (Some "maxime"))
                        ] |> Encode.toString 0

                equal expected actual

            testCase "option without a value `None` works" <| fun _ ->
                let expected = """{"id":1,"operator":null}"""

                let actual =
                    Encode.object
                        [ ("id", Encode.int 1)
                          ("operator", Encode.option Encode.string None)
                        ] |> Encode.toString 0

                equal expected actual
        ]
    ]
