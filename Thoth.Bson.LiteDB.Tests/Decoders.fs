module Tests.Decoders

open Thoth.Json.Net
open Util.Testing
open System
open Expecto
open VerifyExpecto
open VerifyTests
open Argon

let jsonRecord =
    """{ "a": 1.0,
         "b": 2.0,
         "c": 3.0,
         "d": 4.0,
         "e": 5.0,
         "f": 6.0,
         "g": 7.0,
         "h": 8.0 }"""

let jsonRecordInvalid =
    """{ "a": "invalid_a_field",
         "b": "invalid_a_field",
         "c": "invalid_a_field",
         "d": "invalid_a_field",
         "e": "invalid_a_field",
         "f": "invalid_a_field",
         "g": "invalid_a_field",
         "h": "invalid_a_field" }"""

open Tests.Types

type RecordWithPrivateConstructor = private { Foo1: int; Foo2: float }
type UnionWithPrivateConstructor = private Bar of string | Baz
type UnionWithMultipleFields = Multi of string * int * float

let tests : Test =
    VerifierSettings.AddExtraSettings(fun settings -> settings.AddFSharpConverters())

    testList "Thoth.Json.Decode" [

        testList "Errors" [

            #if FABLE_COMPILER

            testCase "circular structure are supported when reporting error" <| fun _ ->
                let a = createObj [ ]
                let b = createObj [ ]
                a?child <- b
                b?child <- a

                let expected : Result<float, string> = Error "Error at: `$`\nExpecting a float but decoder failed. Couldn\'t report given value due to circular structure. "
                let actual = Decode.fromValue "$" Decode.float b

                equal expected actual
            #endif

            testTask "invalid json" {
                let actual = Decode.fromString Decode.float "maxime"

                do! Verifier.Verify("invalid json", actual).ToTask()
            }

            testTask "invalid json #3 - Special case for Thoth.Json.Net" {
                // See: https://github.com/thoth-org/Thoth.Json.Net/pull/48
                let incorrectJson = """
                {
                "Ab": [
                    "RecordC",
                    {
                    "C1": "",
                    "C2": "",
                """

                let actual = Decode.fromString Decode.float incorrectJson

                do! Verifier.Verify("invalid json #3 - Special case for Thoth.Json.Net", actual).ToTask()
            }

            testCase "user exceptions are not captured by the decoders" <| fun _ ->
                let expected = true

                let decoder =
                    (fun _ _ ->
                        raise CustomException
                    )

                let actual =
                    try
                        Decode.fromString decoder "\"maxime\""
                        |> ignore // Ignore the result as we only want to trigger the decoder and capture the exception
                        false
                    with
                        | CustomException ->
                            true

                equal expected actual
        ]

        testList "Primitives" [

            testCase "unit works" <| fun _ ->
                let expected = Ok ()
                let actual =
                    Decode.fromString Decode.unit "null"

                equal expected actual

            testCase "a string works" <| fun _ ->
                let expected = Ok("maxime")
                let actual =
                    Decode.fromString Decode.string "\"maxime\""

                equal expected actual

            testCase "a string with new line works" <| fun _ ->
                let expected = Ok("a\nb")
                let actual =
                    Decode.fromString Decode.string "\"a\\nb\""

                equal expected actual

            testCase "a string with new line character works" <| fun _ ->
                let expected = Ok("a\\nb")
                let actual =
                    Decode.fromString Decode.string "\"a\\\\nb\""

                equal expected actual

            testCase "a string with tab works" <| fun _ ->
                let expected = Ok("a\tb")
                let actual =
                    Decode.fromString Decode.string "\"a\\tb\""

                equal expected actual

            testCase "a string with tab character works" <| fun _ ->
                let expected = Ok("a\\tb")
                let actual =
                    Decode.fromString Decode.string "\"a\\\\tb\""

                equal expected actual

            testCase "a char works" <| fun _ ->
                let expected = Ok('a')
                let actual =
                    Decode.fromString Decode.char "\"a\""

                equal expected actual

            testTask "a char reports an error if there are more than 1 characters in the string" {
                let actual =
                    Decode.fromString Decode.char "\"ab\""

                do! Verifier.Verify("a char reports an error if there are more than 1 characters in the string", actual).ToTask()
            }

            testCase "a float works" <| fun _ ->
                let expected = Ok(1.2)
                let actual =
                    Decode.fromString Decode.float "1.2"

                equal expected actual

            testCase "a float from int works" <| fun _ ->
                let expected = Ok(1.0)
                let actual =
                    Decode.fromString Decode.float "1"

                equal expected actual

            testCase "a bool works" <| fun _ ->
                let expected = Ok(true)
                let actual =
                    Decode.fromString Decode.bool "true"

                equal expected actual

            testTask "an invalid bool output an error" {
                let actual =
                    Decode.fromString Decode.bool "2"

                do! Verifier.Verify("an invalid bool output an error", actual).ToTask()
            }

            testCase "an int works" <| fun _ ->
                let expected = Ok(25)
                let actual =
                    Decode.fromString Decode.int "25"

                equal expected actual

            testTask "an invalid int [invalid range: too big] output an error" {
                let actual =
                    Decode.fromString Decode.int "2147483648"

                do! Verifier.Verify("an invalid int [invalid range too big] output an error", actual).ToTask()
            }

            testTask "an invalid int [invalid range: too small] output an error" {
                let actual =
                    Decode.fromString Decode.int "-2147483649"

                do! Verifier.Verify("an invalid int [invalid range too small] output an error", actual).ToTask()
            }

            testCase "an int16 works from number" <| fun _ ->
                let expected = Ok(int16 25)
                let actual =
                    Decode.fromString Decode.int16 "25"

                equal expected actual

            testCase "an int16 works from string" <| fun _ ->
                let expected = Ok(int16 -25)
                let actual =
                    Decode.fromString Decode.int16 "\"-25\""

                equal expected actual

            testTask "an int16 output an error if value is too big" {
                let actual =
                    Decode.fromString Decode.int16 "32768"

                do! Verifier.Verify("an int16 output an error if value is too big", actual).ToTask()
            }

            testTask "an int16 output an error if value is too small" {
                let actual =
                    Decode.fromString Decode.int16 "-32769"

                do! Verifier.Verify("an int16 output an error if value is too small", actual).ToTask()
            }

            testTask "an int16 output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.int16 "\"maxime\""

                do! Verifier.Verify("an int16 output an error if incorrect string", actual).ToTask()
            }

            testCase "an uint16 works from number" <| fun _ ->
                let expected = Ok(uint16 25)
                let actual =
                    Decode.fromString Decode.uint16 "25"

                equal expected actual

            testCase "an uint16 works from string" <| fun _ ->
                let expected = Ok(uint16 25)
                let actual =
                    Decode.fromString Decode.uint16 "\"25\""

                equal expected actual

            testTask "an uint16 output an error if value is too big" {
                let actual =
                    Decode.fromString Decode.uint16 "65536"

                do! Verifier.Verify("an uint16 output an error if value is too big", actual).ToTask()
            }

            testTask "an uint16 output an error if value is too small" {
                let actual =
                    Decode.fromString Decode.uint16 "-1"

                do! Verifier.Verify("an uint16 output an error if value is too small", actual).ToTask()
            }

            testTask "an uint16 output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.uint16 "\"maxime\""

                do! Verifier.Verify("an uint16 output an error if incorrect string", actual).ToTask()
            }

            testCase "an int64 works from number" <| fun _ ->
                let expected = Ok 1000L
                let actual =
                    Decode.fromString Decode.int64 "1000"

                equal expected actual

            testCase "an int64 works from string" <| fun _ ->
                let expected = Ok 99L
                let actual =
                    Decode.fromString Decode.int64 "\"99\""

                equal expected actual

            testTask "an int64 works output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.int64 "\"maxime\""

                do! Verifier.Verify("an int64 works output an error if incorrect string", actual).ToTask()
            }

            testCase "an uint32 works from number" <| fun _ ->
                let expected = Ok 1000u
                let actual =
                    Decode.fromString Decode.uint32 "1000"

                equal expected actual

            testCase "an uint32 works from string" <| fun _ ->
                let expected = Ok 1000u
                let actual =
                    Decode.fromString Decode.uint32 "\"1000\""

                equal expected actual

            testTask "an uint32 output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.uint32 "\"maxime\""

                do! Verifier.Verify("an uint32 output an error if incorrect string", actual).ToTask()
            }

            testCase "an uint64 works from number" <| fun _ ->
                let expected = Ok 1000UL
                let actual =
                    Decode.fromString Decode.uint64 "1000"

                equal expected actual

            testCase "an uint64 works from string" <| fun _ ->
                let expected = Ok 1000UL
                let actual =
                    Decode.fromString Decode.uint64 "\"1000\""

                equal expected actual

            testTask "an uint64 output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.uint64 "\"maxime\""

                do! Verifier.Verify("an uint64 output an error if incorrect string", actual).ToTask()
            }

            testCase "a byte works from number" <| fun _ ->
                let expected = Ok 25uy
                let actual =
                    Decode.fromString Decode.byte "25"

                equal expected actual

            testCase "a byte works from string" <| fun _ ->
                let expected = Ok 25uy
                let actual =
                    Decode.fromString Decode.byte "\"25\""

                equal expected actual

            testTask "a byte output an error if value is too big" {
                let actual =
                    Decode.fromString Decode.byte "256"

                do! Verifier.Verify("a byte output an error if value is too big", actual).ToTask()
            }

            testTask "a byte output an error if value is too small" {
                let actual =
                    Decode.fromString Decode.byte "-1"

                do! Verifier.Verify("a byte output an error if value is too small", actual).ToTask()
            }

            testTask "a byte output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.byte "\"maxime\""

                do! Verifier.Verify("a byte output an error if incorrect string", actual).ToTask()
            }

            testCase "a sbyte works from number" <| fun _ ->
                let expected = Ok 25y
                let actual =
                    Decode.fromString Decode.sbyte "25"

                equal expected actual

            testCase "a sbyte works from string" <| fun _ ->
                let expected = Ok -25y
                let actual =
                    Decode.fromString Decode.sbyte "\"-25\""

                equal expected actual

            testTask "a sbyte output an error if value is too big" {
                let actual =
                    Decode.fromString Decode.sbyte "128"

                do! Verifier.Verify("a sbyte output an error if value is too big", actual).ToTask()
            }

            testTask "a sbyte output an error if value is too small" {
                let actual =
                    Decode.fromString Decode.sbyte "-129"

                do! Verifier.Verify("a sbyte output an error if value is too small", actual).ToTask()
            }

            testTask "a sbyte output an error if incorrect string" {
                let actual =
                    Decode.fromString Decode.sbyte "\"maxime\""

                do! Verifier.Verify("a sbyte output an error if incorrect string", actual).ToTask()
            }

            testCase "an bigint works from number" <| fun _ ->
                let expected = Ok 12I
                let actual =
                    Decode.fromString Decode.bigint "12"

                equal expected actual

            testCase "an bigint works from string" <| fun _ ->
                let expected = Ok 12I
                let actual =
                    Decode.fromString Decode.bigint "\"12\""

                equal expected actual

            testTask "an bigint output an error if invalid string" {
                let actual =
                    Decode.fromString Decode.bigint "\"maxime\""

                do! Verifier.Verify("an bigint output an error if invalid string", actual).ToTask()
            }

            testCase "a string representing a DateTime should be accepted as a string" <| fun _ ->
                let expected = "2018-10-01T11:12:55.00Z"
                let actual =
                    Decode.fromString Decode.string "\"2018-10-01T11:12:55.00Z\""

                equal (Ok expected) actual

            testCase "a datetime works" <| fun _ ->
                let expected = new DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc)
                let actual =
                    Decode.fromString Decode.datetimeUtc "\"2018-10-01T11:12:55.00Z\""

                equal (Ok expected) actual

            testCase "a non-UTC datetime works" <| fun _ ->
                let expected = new DateTime(2018, 10, 1, 11, 12, 55)
                let actual =
                    Decode.fromString Decode.datetimeLocal "\"2018-10-01T11:12:55\""

                equal (Ok expected) actual

            testTask "a datetime output an error if invalid string" {
                let actual =
                    Decode.fromString Decode.datetimeUtc "\"invalid_string\""

                do! Verifier.Verify("a datetime output an error if invalid string", actual).ToTask()
            }

            testCase "a datetime works with TimeZone" <| fun _ ->
                let localDate = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Local)

                let expected = Ok (localDate.ToUniversalTime())
                let json = sprintf "\"%s\"" (localDate.ToString("O"))
                let actual =
                    Decode.fromString Decode.datetimeUtc json

                equal expected actual

            testCase "a datetimeOffset works" <| fun _ ->
                let expected =
                    DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
                    |> Ok
                let json = "\"2018-07-02T12:23:45+02:00\""
                let actual =
                    Decode.fromString Decode.datetimeOffset json
                equal expected actual

            testTask "a datetimeOffset returns Error if invalid format" {
                let json = "\"NOT A DATETIMEOFFSET\""
                let actual =
                    Decode.fromString Decode.datetimeOffset json

                do! Verifier.Verify("a datetimeOffset returns Error if invalid format", actual).ToTask()
            }

            testCase "a timespan works" <| fun _ ->
                let expected =
                    TimeSpan(23, 45, 0)
                    |> Ok
                let json = "\"23:45:00\""
                let actual =
                    Decode.fromString Decode.timespan json
                equal expected actual

            testTask "a timespan returns Error if invalid format" {
                let json = "\"NOT A TimeSpan\""
                let actual =
                    Decode.fromString Decode.timespan json

                do! Verifier.Verify("a timespan returns Error if invalid format", actual).ToTask()
            }

            testCase "an enum<sbyte> works" <| fun _ ->
                let expected = Ok Enum_Int8.NinetyNine
                let actual =
                    Decode.fromString Decode.Enum.sbyte "99"

                equal expected actual

            testCase "an enum<byte> works" <| fun _ ->
                let expected = Ok Enum_UInt8.NinetyNine
                let actual =
                    Decode.fromString Decode.Enum.byte "99"

                equal expected actual

            testCase "an enum<int> works" <| fun _ ->
                let expected = Ok Enum_Int.One
                let actual =
                    Decode.fromString Decode.Enum.int "1"

                equal expected actual

            testCase "an enum<uint32> works" <| fun _ ->
                let expected = Ok Enum_UInt32.NinetyNine
                let actual =
                    Decode.fromString Decode.Enum.uint32 "99"

                equal expected actual

            testCase "an enum<int16> works" <| fun _ ->
                let expected = Ok Enum_Int16.NinetyNine
                let actual =
                    Decode.fromString Decode.Enum.int16 "99"

                equal expected actual

            testCase "an enum<uint16> works" <| fun _ ->
                let expected = Ok Enum_UInt16.NinetyNine
                let actual =
                    Decode.fromString Decode.Enum.uint16 "99"

                equal expected actual

        ]

        testList "Tuples" [
            testCase "tuple2 works" <| fun _ ->
                let json = """[1, "maxime"]"""
                let expected = Ok(1, "maxime")

                let actual =
                    Decode.fromString (Decode.tuple2 Decode.int Decode.string) json

                equal expected actual

            testCase "tuple3 works" <| fun _ ->
                let json = """[1, "maxime", 2.5]"""
                let expected = Ok(1, "maxime", 2.5)

                let actual =
                    Decode.fromString
                        (Decode.tuple3
                            Decode.int
                            Decode.string
                            Decode.float) json

                equal expected actual

            testCase "tuple4 works" <| fun _ ->
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }]"""
                let expected = Ok(1, "maxime", 2.5, { fieldA = "test" })

                let actual =
                    Decode.fromString
                        (Decode.tuple4
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder) json

                equal expected actual

            testCase "tuple5 works" <| fun _ ->
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, false]"""
                let expected = Ok(1, "maxime", 2.5, { fieldA = "test" }, false)

                let actual =
                    Decode.fromString
                        (Decode.tuple5
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.bool) json

                equal expected actual

            testCase "tuple6 works" <| fun _ ->
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, false, null]"""
                let expected = Ok(1, "maxime", 2.5, { fieldA = "test" }, false, null)

                let actual =
                    Decode.fromString
                        (Decode.tuple6
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.bool
                            (Decode.nil null)) json

                equal expected actual

            testCase "tuple7 works" <| fun _ ->
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, false, null, 56]"""
                let expected = Ok(1, "maxime", 2.5, { fieldA = "test" }, false, null, 56)

                let actual =
                    Decode.fromString
                        (Decode.tuple7
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.bool
                            (Decode.nil null)
                            Decode.int) json

                equal expected actual

            testCase "tuple8 works" <| fun _ ->
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, false, null, true, 98]"""
                let expected = Ok(1, "maxime", 2.5, { fieldA = "test" }, false, null, true, 98)

                let actual =
                    Decode.fromString
                        (Decode.tuple8
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.bool
                            (Decode.nil null)
                            Decode.bool
                            Decode.int) json

                equal expected actual

            testTask "tuple2 returns an error if invalid json" {
                let json = """[1, false, "unused value"]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple2
                            Decode.int
                            Decode.string) json

                do! Verifier.Verify("tuple2 returns an error if invalid json", actual).ToTask()
            }

            testTask "tuple3 returns an error if invalid json" {
                let json = """[1, "maxime", false]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple3
                            Decode.int
                            Decode.string
                            Decode.float) json

                do! Verifier.Verify("tuple3 returns an error if invalid json", actual).ToTask()
            }

            testTask "tuple4 returns an error if invalid json (missing index)" {
                let json = """[1, "maxime", 2.5]"""
                let actual =
                    Decode.fromString
                        (Decode.tuple4
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder) json

                do! Verifier.Verify("tuple4 returns an error if invalid json (missing index)", actual).ToTask()
            }

            testTask "tuple4 returns an error if invalid json (error in the nested object)" {
                let json = """[1, "maxime", 2.5, { "fieldA" : false }]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple4
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder) json

                do! Verifier.Verify("tuple4 returns an error if invalid json (error in the nested object)", actual).ToTask()
            }

            testTask "tuple5 returns an error if invalid json" {
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, false]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple5
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.datetimeUtc) json

                do! Verifier.Verify("tuple5 returns an error if invalid json", actual).ToTask()
            }

            testTask "tuple6 returns an error if invalid json" {
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, "2018-10-01T11:12:55.00Z", false]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple6
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.datetimeUtc
                            (Decode.nil null)) json

                do! Verifier.Verify("tuple6 returns an error if invalid json", actual).ToTask()
            }

            testTask "tuple7 returns an error if invalid json" {
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, "2018-10-01T11:12:55.00Z", null, false]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple7
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.datetimeUtc
                            (Decode.nil null)
                            Decode.int) json

                do! Verifier.Verify("tuple7 returns an error if invalid json", actual).ToTask()
            }

            testTask "tuple8 returns an error if invalid json" {
                let json = """[1, "maxime", 2.5, { "fieldA" : "test" }, "2018-10-01T11:12:55.00Z", null, 56, "maxime"]"""

                let actual =
                    Decode.fromString
                        (Decode.tuple8
                            Decode.int
                            Decode.string
                            Decode.float
                            SmallRecord.Decoder
                            Decode.datetimeUtc
                            (Decode.nil null)
                            Decode.int
                            Decode.int) json

                do! Verifier.Verify("tuple8 returns an error if invalid json", actual).ToTask()
            }

        ]

        testList "Object primitives" [

            testCase "field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok("maxime")

                let actual =
                    Decode.fromString (Decode.field "name" Decode.string) json

                equal expected actual

            testTask "field output an error explaining why the value is considered invalid" {
                let json = """{ "name": null, "age": 25 }"""

                let actual =
                    Decode.fromString (Decode.field "name" Decode.int) json

                do! Verifier.Verify("field output an error explaining why the value is considered invalid", actual).ToTask()
            }

            testTask "field output an error when field is missing" {
                let json = """{ "name": "maxime", "age": 25 }"""
                let actual =
                    Decode.fromString (Decode.field "height" Decode.float) json

                do! Verifier.Verify("field output an error when field is missing", actual).ToTask()
            }

            testCase "at works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok "maxime"

                let actual =
                    Decode.fromString (Decode.at ["user"; "name"] Decode.string) json

                equal expected actual

            testTask "at output an error if the path failed" {
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let actual =
                    Decode.fromString (Decode.at ["user"; "firstname"] Decode.string) json

                do! Verifier.Verify("at output an error if the path failed", actual).ToTask()
            }

            testTask "at output an error explaining why the value is considered invalid" {
                let json = """{ "name": null, "age": 25 }"""

                let actual =
                    Decode.fromString (Decode.at [ "name" ] Decode.int) json

                do! Verifier.Verify("at output an error explaining why the value is considered invalid", actual).ToTask()
            }

            testCase "index works" <| fun _ ->
                let json = """["maxime", "alfonso", "steffen"]"""
                let expected = Ok("alfonso")

                let actual =
                    Decode.fromString (Decode.index 1 Decode.string) json

                equal expected actual

            testTask "index output an error if array is to small" {
                let json = """["maxime", "alfonso", "steffen"]"""
                let actual =
                    Decode.fromString (Decode.index 5 Decode.string) json

                do! Verifier.Verify("index output an error if array is to small", actual).ToTask()
            }

            testTask "index output an error if value isn't an array" {
                let json = "1"

                let actual =
                    Decode.fromString (Decode.index 5 Decode.string) json

                do! Verifier.Verify("index output an error if value isn't an array", actual).ToTask()
            }

        ]


        testList "Data structure" [

            testCase "list works" <| fun _ ->
                let expected = Ok([1; 2; 3])

                let actual =
                    Decode.fromString (Decode.list Decode.int) "[1, 2, 3]"

                equal expected actual

            testCase "nested lists work" <| fun _ ->
                [ [ "maxime2" ] ]
                |> List.map (fun d ->
                    d
                    |> List.map Encode.string
                    |> Encode.list)
                |> Encode.list
                |> Encode.toString 4
                |> Decode.fromString (Decode.list (Decode.list Decode.string))
                |> function Ok v -> equal [["maxime2"]] v | Error er -> failwith er

            testTask "an invalid list output an error" {
                let actual =
                    Decode.fromString (Decode.list Decode.int) "1"

                do! Verifier.Verify("an invalid list output an error", actual).ToTask()
            }

            testCase "array works" <| fun _ ->
                // Need to pass by a list otherwise Fable use:
                // new Int32Array([1, 2, 3]) and the test fails
                // And this would give:
                // Expected: Result { tag: 0, data: Int32Array [ 1, 2, 3 ] }
                // Actual: Result { tag: 0, data: [ 1, 2, 3 ] }
                let expected = Ok([1; 2; 3] |> List.toArray)

                let actual =
                    Decode.fromString (Decode.array Decode.int) "[1, 2, 3]"

                equal expected actual

            testTask "an invalid array output an error" {
                let actual =
                    Decode.fromString (Decode.array Decode.int) "1"

                do! Verifier.Verify("an invalid array output an error", actual).ToTask()
            }

            testCase "keys works" <| fun _ ->
                let expected = Ok(["a"; "b"; "c"])

                let actual =
                    Decode.fromString Decode.keys """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testTask "keys returns an error for invalid objects" {
                let actual =
                    Decode.fromString Decode.keys "1"

                do! Verifier.Verify("keys returns an error for invalid objects", actual).ToTask()
            }

            testCase "keyValuePairs works" <| fun _ ->
                let expected = Ok([("a", 1) ; ("b", 2) ; ("c", 3)])

                let actual =
                    Decode.fromString (Decode.keyValuePairs Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", 1) ; ("b", 2) ; ("c", 3)]))

                let actual =
                    Decode.fromString (Decode.dict Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict with custom decoder works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", Record2.Create 1. 1.) ; ("b", Record2.Create 2. 2.) ; ("c", Record2.Create 3. 3.)]))

                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString (Decode.dict decodePoint)
                        """
{
    "a":
        {
            "a": 1.0,
            "b": 1.0
        },
    "b":
        {
            "a": 2.0,
            "b": 2.0
        },
    "c":
        {
            "a": 3.0,
            "b": 3.0
        }
}
                        """

                equal expected actual

            testTask "an invalid dict output an error" {
                let actual =
                    Decode.fromString (Decode.dict Decode.int) "1"

                do! Verifier.Verify("an invalid dict output an error", actual).ToTask()
            }

            testCase "map' works" <| fun _ ->
                let expected = Ok(Map.ofList([(1, "x") ; (2, "y") ; (3, "z")]))

                let actual =
                    Decode.fromString (Decode.map' Decode.int Decode.string) """[ [ 1, "x" ], [ 2, "y" ], [ 3, "z" ] ]"""

                equal expected actual

            testCase "map' with custom key decoder works" <| fun _ ->
                let expected = Ok(Map.ofList([ ((1, 6), "a") ; ((2, 7), "b") ; ((3, 8), "c") ]))

                let decodePoint =
                    Decode.map2
                        (fun x y -> x, y)
                        (Decode.field "x" Decode.int)
                        (Decode.field "y" Decode.int)

                let actual =
                    Decode.fromString (Decode.map' decodePoint Decode.string)
                        """
[
    [
        {
            "x": 1,
            "y": 6
        },
        "a"
    ],
    [
        {
            "x": 2,
            "y": 7
        },
        "b"
    ],
    [
        {
            "x": 3,
            "y": 8
        },
        "c"
    ]
]
                        """

                equal expected actual
        ]

        testList "Inconsistent structure" [

            testCase "oneOf works" <| fun _ ->
                let expected = Ok([1; 2; 0; 4])

                let badInt =
                    Decode.oneOf [ Decode.int; Decode.nil 0 ]

                let actual =
                    Decode.fromString (Decode.list badInt) "[1,2,null,4]"

                equal expected actual

            testCase "oneOf works in combination with object builders" <| fun _ ->
                let json = """{ "Bar": { "name": "maxime", "age": 25 } }"""
                let expected = Ok(Choice2Of2 { fieldA = "maxime" })

                let decoder1 =
                    Decode.object (fun get ->
                        { fieldA = get.Required.Field "name" Decode.string })

                let decoder2 =
                    Decode.oneOf [
                        Decode.field "Foo" decoder1 |> Decode.map Choice1Of2
                        Decode.field "Bar" decoder1 |> Decode.map Choice2Of2
                    ]

                let actual =
                    Decode.fromString decoder2 json

                equal expected actual

            testCase "oneOf works with optional" <| fun _ ->
                let decoder =
                    Decode.oneOf
                        [
                            Decode.field "Normal" Decode.float |> Decode.map Normal
                            Decode.field "Reduced" (Decode.option Decode.float) |> Decode.map Reduced
                            Decode.field "Zero" Decode.bool |> Decode.map (fun _ -> Zero)
                        ]

                """{"Normal": 4.5}""" |> Decode.fromString decoder |> equal (Ok(Normal 4.5))
                """{"Reduced": 4.5}""" |> Decode.fromString decoder |> equal (Ok(Reduced(Some 4.5)))
                """{"Reduced": null}""" |> Decode.fromString decoder |> equal (Ok(Reduced None))
                """{"Zero": true}""" |> Decode.fromString decoder |> equal (Ok Zero)

            testTask "oneOf output errors if all case fails" {
                let badInt =
                    Decode.oneOf [ Decode.string; Decode.field "test" Decode.string ]

                let actual =
                    Decode.fromString (Decode.list badInt) "[1,2,null,4]"

                do! Verifier.Verify("oneOf output errors if all case fails", actual).ToTask()
            }

            testCase "optional works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25, "something_undefined": null }"""

                let expectedValid = Ok(Some "maxime")
                let actualValid =
                    Decode.fromString (Decode.optional "name" Decode.string) json

                equal expectedValid actualValid

                match Decode.fromString (Decode.optional "name" Decode.int) json with
                | Error _ -> ()
                | Ok _ -> failwith "Expected type error for `name` field"

                let expectedMissingField = Ok(None)
                let actualMissingField =
                    Decode.fromString (Decode.optional "height" Decode.int) json

                equal expectedMissingField actualMissingField

                let expectedUndefinedField = Ok(None)
                let actualUndefinedField =
                    Decode.fromString (Decode.optional "something_undefined" Decode.string) json

                equal expectedUndefinedField actualUndefinedField

            testTask "optional returns Error value if decoder fails" {
                let json = """{ "name": 12, "age": 25 }"""

                let actual =
                    Decode.fromString (Decode.optional "name" Decode.string) json

                do! Verifier.Verify("optional returns Error value if decoder fails", actual).ToTask()
            }

            testCase "optionalAt works" <| fun _ ->
                let json = """{ "data" : { "name": "maxime", "age": 25, "something_undefined": null } }"""

                let expectedValid = Ok(Some "maxime")
                let actualValid =
                    Decode.fromString (Decode.optionalAt [ "data"; "name" ] Decode.string) json

                equal expectedValid actualValid

                match Decode.fromString (Decode.optionalAt [ "data"; "name" ] Decode.int) json with
                | Error _ -> ()
                | Ok _ -> failwith "Expected type error for `name` field"

                let expectedMissingField = Ok None
                let actualMissingField =
                    Decode.fromString (Decode.optionalAt [ "data"; "height" ] Decode.int) json

                equal expectedMissingField actualMissingField

                let expectedUndefinedField = Ok(None)
                let actualUndefinedField =
                    Decode.fromString (Decode.optionalAt [ "data"; "something_undefined" ] Decode.string) json

                equal expectedUndefinedField actualUndefinedField

                let expectedUndefinedField = Ok(None)
                let actualUndefinedField =
                    Decode.fromString (Decode.optionalAt [ "data"; "something_undefined"; "name" ] Decode.string) json

                equal expectedUndefinedField actualUndefinedField

            testTask "combining field and option decoders works" {
                let json = """{ "name": "maxime", "age": 25, "something_undefined": null }"""

                let expectedValid = Ok(Some "maxime")
                let actualValid =
                    Decode.fromString (Decode.field "name" (Decode.option Decode.string)) json

                equal expectedValid actualValid

                match Decode.fromString (Decode.field "name" (Decode.option Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works name", msg).ToTask()
                | Ok _ -> failwith "Expected type error for `name` field #1"

                match Decode.fromString (Decode.field "this_field_do_not_exist" (Decode.option Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works this_field_do_not_exist", msg).ToTask()
                | Ok _ ->
                    failwith "Expected type error for `name` field #2"

                match Decode.fromString (Decode.field "something_undefined" (Decode.option Decode.int)) json with
                | Error _ -> failwith """`Decode.field "something_undefined" (Decode.option Decode.int)` test should pass"""
                | Ok result -> equal None result

                // Same tests as before but we are calling `option` then `field`

                let expectedValid2 = Ok(Some "maxime")
                let actualValid2 =
                    Decode.fromString (Decode.option (Decode.field "name" Decode.string)) json

                equal expectedValid2 actualValid2

                match Decode.fromString (Decode.option (Decode.field "name" Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works option type", msg).ToTask()
                | Ok _ -> failwith "Expected type error for `name` field #3"

                match Decode.fromString (Decode.option (Decode.field "this_field_do_not_exist" Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works option this_field_do_not_exist", msg).ToTask()
                | Ok _ -> failwith "Expected type error for `name` field #4"

                match Decode.fromString (Decode.option (Decode.field "something_undefined" Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works option something_undefined", msg).ToTask()
                | Ok _ -> failwith "Expected type error for `name` field"

                // Alfonso: Should this test pass? We should use Decode.optional instead
                // - `Decode.fromString (Decode.field "height" (Decode.option Decode.int)) json` == `Ok(None)`
                //
                // Maxime here :)
                // I don't think this test should pass.
                // For me `Decode.field "height" (Decode.option Decode.int)` means:
                // 1. The field `height` is required
                // 2. If `height` exist then, it's value can be `Some X` where `X` is an `int` or `None`
                //
                // I am keep the comments here so we keep track of the explanation if we later need to give it a second though.
                //
                match Decode.fromString (Decode.field "height" (Decode.option Decode.int)) json with
                | Error msg ->
                    do! Verifier.Verify("combining field and option decoders works optional", msg).ToTask()
                | Ok _ -> failwith "Expected type error for `height` field"

                let expectedUndefinedField = Ok(None)
                let actualUndefinedField =
                    Decode.fromString (Decode.field "something_undefined" (Decode.option Decode.string)) json

                equal expectedUndefinedField actualUndefinedField
            }
        ]

        testList "Fancy decoding" [

            testCase "null works (test on an int)" <| fun _ ->
                let expected = Ok(20)
                let actual =
                    Decode.fromString (Decode.nil 20) "null"

                equal expected actual

            testCase "null works (test on a boolean)" <| fun _ ->
                let expected = Ok(false)
                let actual =
                    Decode.fromString (Decode.nil false) "null"

                equal expected actual

            testCase "succeed works" <| fun _ ->
                let expected = Ok(7)
                let actual =
                    Decode.fromString (Decode.succeed 7) "true"

                equal expected actual

            testTask "succeed output an error if the JSON is invalid" {
                let actual =
                    Decode.fromString (Decode.succeed 7) "maxime"

                do! Verifier.Verify("succeed output an error if the JSON is invalid", actual).ToTask()
            }

            testTask "fail works" {
                let msg = "Failing because it's fun"
                let actual =
                    Decode.fromString (Decode.fail msg) "true"

                do! Verifier.Verify("fail works", actual).ToTask()
            }

            testCase "andMap works for any arity" <| fun _ ->
                // In the past maximum arity in Fable was 8
                let json =
                    """{"a": 1,"b": 2,"c": 3,"d": 4,"e": 5,"f": 6,"g": 7,"h": 8,"i": 9,"j": 10,"k": 11}"""

                let decodeRecord10 =
                    Decode.succeed Record10.Create
                        |> Decode.andMap (Decode.field "a" Decode.int)
                        |> Decode.andMap (Decode.field "b" Decode.int)
                        |> Decode.andMap (Decode.field "c" Decode.int)
                        |> Decode.andMap (Decode.field "d" Decode.int)
                        |> Decode.andMap (Decode.field "e" Decode.int)
                        |> Decode.andMap (Decode.field "f" Decode.int)
                        |> Decode.andMap (Decode.field "g" Decode.int)
                        |> Decode.andMap (Decode.field "h" Decode.int)
                        |> Decode.andMap (Decode.field "i" Decode.int)
                        |> Decode.andMap (Decode.field "j" Decode.int)
                        |> Decode.andMap (Decode.field "k" Decode.int)

                let actual =
                    Decode.fromString decodeRecord10 json

                let expected = Ok { a = 1; b = 2; c = 3; d = 4; e = 5; f = 6; g = 7; h = 8; i = 9; j = 10; k = 11 }

                equal expected actual

            testCase "andThen works" <| fun _ ->
                let expected = Ok 1
                let infoHelp version =
                    match version with
                    | 4 ->
                        Decode.succeed 1
                    | 3 ->
                        Decode.succeed 1
                    | _ ->
                        Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

                let info : Decoder<int> =
                    Decode.field "version" Decode.int
                    |> Decode.andThen infoHelp

                let actual =
                    Decode.fromString info """{ "version": 3, "data": 2 }"""

                equal expected actual


            testTask "andThen generate an error if an error occuered" {
                let infoHelp version : Decoder<int> =
                    match version with
                    | 4 ->
                        Decode.succeed 1
                    | 3 ->
                        Decode.succeed 1
                    | _ ->
                        Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

                let info =
                    Decode.field "version" Decode.int
                    |> Decode.andThen infoHelp

                let actual =
                    Decode.fromString info """{ "info": 3, "data": 2 }"""

                do! Verifier.Verify("andThen generate an error if an error occuered", actual).ToTask()
            }

            testCase "all works" <| fun _ ->
                let expected = Ok [1; 2; 3]

                let decodeAll = Decode.all [
                    Decode.succeed 1
                    Decode.succeed 2
                    Decode.succeed 3
                ]

                let actual = Decode.fromString decodeAll "{}"

                equal expected actual

            testCase "combining Decode.all and Decode.keys works" <| fun _ ->
                let expected = Ok [1; 2; 3]

                let decoder =
                    Decode.keys
                    |> Decode.andThen (fun keys ->
                        keys
                        |> List.except ["special_property"]
                        |> List.map (fun key -> Decode.field key Decode.int)
                        |> Decode.all)

                let actual = Decode.fromString decoder """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "all succeeds on empty lists" <| fun _ ->
                let expected = Ok []

                let decodeNone = Decode.all []

                let actual = Decode.fromString decodeNone "{}"

                equal expected actual


            testTask "all fails when one decoder fails" {
                let decodeAll = Decode.all [
                    Decode.succeed 1
                    Decode.int
                    Decode.succeed 3
                ]

                let actual = Decode.fromString decodeAll "{}"

                do! Verifier.Verify("all fails when one decoder fails", actual).ToTask()
            }
        ]

        testList "Mapping" [

            testCase "map works" <| fun _ ->
                let expected = Ok(6)
                let stringLength =
                    Decode.map String.length Decode.string

                let actual =
                    Decode.fromString stringLength "\"maxime\""
                equal expected actual


            testCase "map2 works" <| fun _ ->
                let expected = Ok({a = 1.; b = 2.} : Record2)

                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map3 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3. } : Record3)

                let decodePoint =
                    Decode.map3 Record3.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map4 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4. } : Record4)

                let decodePoint =
                    Decode.map4 Record4.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map5 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5. } : Record5)

                let decodePoint =
                    Decode.map5 Record5.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map6 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6. } : Record6)

                let decodePoint =
                    Decode.map6 Record6.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map7 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6.
                                    g = 7. } : Record7)

                let decodePoint =
                    Decode.map7 Record7.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)
                        (Decode.field "g" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map8 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6.
                                    g = 7.
                                    h = 8. } : Record8)

                let decodePoint =
                    Decode.map8 Record8.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)
                        (Decode.field "g" Decode.float)
                        (Decode.field "h" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testTask "map2 generate an error if invalid" {
                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecordInvalid

                do! Verifier.Verify("map2 generate an error if invalid", actual).ToTask()
            }

        ]

        testList "object builder" [

            testCase "get.Required.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Required.Field returns Error if field is missing" {
                let json = """{ "age": 25 }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Required.Field returns Error if field is missing", actual).ToTask()
            }

            testTask "get.Required.Field returns Error if type is incorrect" {
                let json = """{ "name": 12, "age": 25 }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Required.Field returns Error if type is incorrect", actual).ToTask()
            }

            testCase "get.Optional.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None value if field is missing" <| fun _ ->
                let json = """{ "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None if field is null" <| fun _ ->
                let json = """{ "name": null, "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Optional.Field returns Error value if decoder fails" {
                let json = """{ "name": 12, "age": 25 }"""

                let decoder = Decode.object (fun get ->
                    { optionalField = get.Optional.Field "name" Decode.string })

                let actual = Decode.fromString decoder json

                do! Verifier.Verify("get.Optional.Field returns Error value if decoder fails", actual).ToTask()
            }

            testCase "nested get.Optional.Field > get.Required.Field returns None if field is null" <| fun _ ->
                let json = """{ "user": null, "field2": 25 }"""
                let expected = Ok({ User = None; Field2 = 25 })

                let userDecoder =
                    Decode.object
                        (fun get ->
                            { Id = get.Required.Field "id" Decode.int
                              Name = get.Required.Field "name" Decode.string
                              Email = get.Required.Field "email" Decode.string
                              Followers = 0 }
                        )

                let decoder =
                    Decode.object
                        (fun get ->
                            { User = get.Optional.Field "user" userDecoder
                              Field2 = get.Required.Field "field2" Decode.int }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Optional.Field returns Error if type is incorrect" {
                let json = """{ "name": 12, "age": 25 }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Optional.Field returns Error if type is incorrect", actual).ToTask()
            }

            testCase "get.Required.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Required.At returns Error if non-object in path" {
                let json = """{ "user": "maxime" }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Required.At returns Error if non-object in path", actual).ToTask()
            }

            testTask "get.Required.At returns Error if field missing" {
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "firstname" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Required.At returns Error if field missing", actual).ToTask()
            }

            testTask "get.Required.At returns Error if type is incorrect" {
                let json = """{ "user": { "name": 12, "age": 25 } }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Required.At returns Error if type is incorrect", actual).ToTask()
            }

            testCase "get.Optional.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Optional.At returns 'type error' if non-object in path" {
                let json = """{ "user": "maxime" }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Optional.At returns 'type error' if non-object in path", actual).ToTask()
            }

            testCase "get.Optional.At returns None if field missing" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "firstname" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testTask "get.Optional.At returns Error if type is incorrect" {
                let json = """{ "user": { "name": 12, "age": 25 } }"""

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("get.Optional.At returns Error if type is incorrect", actual).ToTask()
            }

            testCase "complex object builder works" <| fun _ ->
                let expected =
                    Ok(User.Create 67 "" "user@mail.com" 0)

                let userDecoder =
                    Decode.object
                        (fun get ->
                            { Id = get.Required.Field "id" Decode.int
                              Name = get.Optional.Field "name" Decode.string
                                        |> Option.defaultValue ""
                              Email = get.Required.Field "email" Decode.string
                              Followers = 0 }
                        )

                let actual =
                    Decode.fromString
                        userDecoder
                        """{ "id": 67, "email": "user@mail.com" }"""

                equal expected actual

            testCase "get.Field.Raw works" <| fun _ ->
                let json = """{
    "enabled": true,
	"shape": "circle",
    "radius": 20
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Required.Raw shapeDecoder } : MyObj
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                let expected =
                    Ok ({ Enabled = true
                          Shape = Circle 20 } : MyObj)

                equal expected actual

            testTask "get.Field.Raw returns Error if a decoder fail" {
                let json = """{
    "enabled": true,
	"shape": "custom_shape",
    "radius": 20
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Required.Raw shapeDecoder } : MyObj
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                do! Verifier.Verify("get.Field.Raw returns Error if a decoder fail", actual).ToTask()
            }

            testTask "get.Field.Raw returns Error if a field is missing in the 'raw decoder'" {
                let json = """{
    "enabled": true,
	"shape": "circle"
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Required.Raw shapeDecoder } : MyObj
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                do! Verifier.Verify("get.Field.Raw returns Error if a field is missing in the 'raw decoder'", actual).ToTask()
            }

            testCase "get.Optional.Raw works" <| fun _ ->
                let json = """{
    "enabled": true,
	"shape": "circle",
    "radius": 20
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Optional.Raw shapeDecoder }
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                let expected =
                    Ok { Enabled = true
                         Shape = Some (Circle 20) }

                equal expected actual

            testCase "get.Optional.Raw returns None if a field is missing" <| fun _ ->
                let json = """{
    "enabled": true,
	"shape": "circle"
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Optional.Raw shapeDecoder }
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                let expected =
                    Ok { Enabled = true
                         Shape = None }

                equal expected actual

            testTask "get.Optional.Raw returns an Error if a decoder fail" {
                let json = """{
    "enabled": true,
	"shape": "invalid_shape"
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Optional.Raw shapeDecoder }
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                do! Verifier.Verify("get.Optional.Raw returns an Error if a decoder fail", actual).ToTask()
            }

            testTask "get.Optional.Raw returns an Error if the type is invalid" {
                let json = """{
    "enabled": true,
	"shape": "circle",
    "radius": "maxime"
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Optional.Raw shapeDecoder }
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                do! Verifier.Verify("get.Optional.Raw returns an Error if the type is invalid", actual).ToTask()
            }

            testCase "get.Optional.Raw returns None if a decoder fails with null" <| fun _ ->
                let json = """{
    "enabled": true,
	"shape": null
}"""
                let shapeDecoder =
                    Decode.field "shape" Decode.string
                    |> Decode.andThen (function
                        | "circle" ->
                            Shape.DecoderCircle
                        | "rectangle" ->
                            Shape.DecoderRectangle
                        | shape ->
                            Decode.fail (sprintf "Unknown shape type %s" shape))

                let decoder =
                    Decode.object (fun get ->
                        { Enabled = get.Required.Field "enabled" Decode.bool
                          Shape = get.Optional.Raw shapeDecoder }
                    )

                let actual =
                    Decode.fromString
                        decoder
                        json

                let expected =
                     Ok { Enabled = true
                          Shape = None }

                equal expected actual

            testTask "Object builders returns all the Errors" {
                let json = """{ "age": 25, "fieldC": "not_a_number", "fieldD": { "sub_field": "not_a_boolean" } }"""

                let decoder =
                    Decode.object (fun get ->
                        { FieldA = get.Required.Field "missing_field_1" Decode.string
                          FieldB = get.Required.At [ "missing_field_2"; "sub_field" ] Decode.string
                          FieldC = get.Optional.Field "fieldC" Decode.int
                                    |> Option.defaultValue -1
                          FieldD = get.Optional.At [ "fieldD"; "sub_field" ] Decode.bool
                                    |> Option.defaultValue false }
                    )

                let actual =
                    Decode.fromString decoder json

                do! Verifier.Verify("Object builders returns all the Errors", actual).ToTask()
            }

            testCase "Test" <| fun _ ->
                let json =
                    """
                    {
                        "person": {
                            "name": "maxime"
                        },
                        "post": null
                    }
                    """

                let personDecoder : Decoder<Person> =
                    Decode.object (fun get ->
                        {
                            Name = get.Required.Field "name" Decode.string
                        }
                    )

                let postDecoder : Decoder<Post> =
                    Decode.object (fun get ->
                        let title = get.Required.Field "title" Decode.string

                        // Accessing the value and doing something with it
                        // To reproduce bug reported in:
                        // https://github.com/thoth-org/Thoth.Json.Net/issues/53
                        title
                        |> Seq.head
                        |> printfn "Title: %A"

                        {
                            Title = title
                        }
                    )

                let dataDecoder =
                    Decode.object (fun get ->
                        {
                            Person = get.Required.Field "person" personDecoder
                            Post = get.Optional.Field "post" postDecoder
                        }
                    )

                let actual = Decode.fromString dataDecoder json
                let expected = Ok { Person = { Name = "maxime" }; Post = None }

                equal expected actual
        ]

    ]
