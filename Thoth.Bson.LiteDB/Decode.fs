namespace Thoth.Json.Net

[<RequireQualifiedAccess>]
module Decode =

    open System.Globalization
    open LiteDB
    open System.IO

    type JTokenType = LiteDB.BsonType
    module Helpers =
        let anyToString (token: JsonValue) : string =
            if isNull token then "null"
            else
                LiteDB.JsonSerializer.Serialize(token, true)


        let inline getField (fieldName: string) (token: JsonValue) = token.Item(fieldName)
        let inline isBool (token: JsonValue) = not(isNull token) && token.Type = JTokenType.Boolean
        let inline isNumber (token: JsonValue) = not(isNull token) && (token.Type = JTokenType.Decimal || token.Type = JTokenType.Double || token.Type = JTokenType.Int32  || token.Type = JTokenType.Int64)
        let inline isIntegralValue (token: JsonValue) = not(isNull token) && (token.Type = JTokenType.Int32 || token.Type = JTokenType.Int64)
        let inline isInteger (token: JsonValue) = not(isNull token) && (token.Type = JTokenType.Int32 || token.Type = JTokenType.Int64)
        let inline isString (token: JsonValue) = not(isNull token) && token.Type = JTokenType.String
        let inline isGuid (token: JsonValue) = not(isNull token) && token.Type = JTokenType.Guid
        let inline isDate (token: JsonValue) = not(isNull token) && token.Type = JTokenType.DateTime
        let inline isArray (token: JsonValue) = not(isNull token) && token.Type = JTokenType.Array
        let inline isObject (token: JsonValue) = not(isNull token) && token.Type = JTokenType.Document
        let inline isUndefined (token: JsonValue) = isNull token
        let inline isNullValue (token: JsonValue) = isNull token || token.Type = JTokenType.Null
        let inline asBool (token: JsonValue): bool = token.AsBoolean
        let inline asInt (token: JsonValue): int = token.AsInt32
        let inline asInt64 (token: JsonValue): int64 = token.AsInt64
        let inline asFloat (token: JsonValue): float = token.AsDouble
        let inline asFloat32 (token: JsonValue): float32 = token.AsDouble |> float32
        let inline asDecimal (token: JsonValue): System.Decimal = token.AsDecimal
        let inline asString (token: JsonValue): string = token.AsString
        let inline asArray (token: JsonValue): JsonValue[] = token.AsArray |> Seq.toArray

    let private genericMsg msg value newLine =
        try
            "Expecting "
                + msg
                + " but instead got:"
                + (if newLine then "\n" else " ")
                + (Helpers.anyToString value)
        with
            | _ ->
                "Expecting "
                + msg
                + " but decoder failed. Couldn't report given value due to circular structure."
                + (if newLine then "\n" else " ")

    let private errorToString (path : string, error) =
        let reason =
            match error with
            | BadPrimitive (msg, value) ->
                genericMsg msg value false
            | BadType (msg, value) ->
                genericMsg msg value true
            | BadPrimitiveExtra (msg, value, reason) ->
                genericMsg msg value false + "\nReason: " + reason
            | BadField (msg, value) ->
                genericMsg msg value true
            | BadPath (msg, value, fieldName) ->
                genericMsg msg value true + ("\nNode `" + fieldName + "` is unkown.")
            | TooSmallArray (msg, value) ->
                "Expecting " + msg + ".\n" + (Helpers.anyToString value)
            | BadOneOf messages ->
                "The following errors were found:\n\n" + String.concat "\n\n" messages
            | FailMessage msg ->
                "The following `failure` occurred with the decoder: " + msg

        match error with
        | BadOneOf _ ->
            // Don't need to show the path here because each error case will show it's own path
            reason
        | _ ->
            "Error at: `" + path + "`\n" + reason

    ///////////////
    // Runners ///
    /////////////

    /// <summary>
    /// Runs the decoder against the given JSON value.
    ///
    /// If the decoder fails, it reports the error prefixed with the given path.
    ///
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// module Decode =
    ///     let fromRootValue (decoder : Decoder&lt;'T&gt;) =
    ///         Decode.fromValue "$" decoder
    /// </code>
    /// </example>
    /// <param name="path">Path used to report the error</param>
    /// <param name="decoder">Decoder to apply</param>
    /// <param name="value">JSON value to decoder</param>
    /// <returns>
    /// Returns <c>Ok</c> if the decoder succeeds, otherwise <c>Error</c> with the error message.
    /// </returns>
    let fromValue (path : string) (decoder : Decoder<'T>) =
        fun value ->
            match decoder path value with
            | Ok success ->
                Ok success
            | Error error ->
                Error (errorToString error)

    /// <summary>
    /// Parse the provided string in as JSON and then run the decoder against it.
    /// </summary>
    /// <param name="decoder">Decoder to apply</param>
    /// <param name="value">JSON string to decode</param>
    /// <returns>
    /// Returns <c>Ok</c> if the decoder succeeds, otherwise <c>Error</c> with the error message.
    /// </returns>
    let fromString (decoder : Decoder<'T>) =
        fun value ->
            try


                let res = LiteDB.JsonSerializer.Deserialize(value:string)

                fromValue "$" decoder res
            with
                | :? LiteException as ex ->
                    Error("Given an invalid JSON: " + ex.Message)

    /// <summary>
    /// Parse the provided string in as JSON and then run the decoder against it.
    /// </summary>
    /// <param name="decoder">Decoder to apply</param>
    /// <param name="value">JSON string to decode</param>
    /// <returns>
    /// Return the decoded value if the decoder succeeds, otherwise throws an exception.
    /// </returns>
    let unsafeFromString (decoder : Decoder<'T>) =
        fun value ->
            match fromString decoder value with
            | Ok x -> x
            | Error msg -> failwith msg

    [<System.Obsolete("Please use fromValue instead")>]
    let decodeValue (path : string) (decoder : Decoder<'T>) = fromValue path decoder

    [<System.Obsolete("Please use fromString instead")>]
    let decodeString (decoder : Decoder<'T>) = fromString decoder

    //////////////////
    // Primitives ///
    ////////////////

    let string : Decoder<string> =
        fun path token ->
            if Helpers.isString token then
                Ok(Helpers.asString token)
            else
                (path, BadPrimitive("a string", token)) |> Error

    let char : Decoder<char> =
        fun path value ->
            if Helpers.isString value then
                let str = Helpers.asString value
                if str.Length = 1 then
                    Ok(str.[0])
                else
                    (path, BadPrimitive("a single character string", value)) |> Error
            else
                (path, BadPrimitive("a char", value)) |> Error

    let guid : Decoder<System.Guid> =
        fun path value ->
            // Using Helpers.isString fails because Json.NET directly assigns Guid type
            if Helpers.isGuid value then
                value.AsGuid |> Ok
            elif Helpers.isString value then
                match System.Guid.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a guid", value)) |> Error
            else (path, BadPrimitive("a guid", value)) |> Error

    let unit : Decoder<unit> =
        fun path value ->
            if Helpers.isNullValue value then
                Ok ()
            else
                (path, BadPrimitive("null", value)) |> Error

    let inline private integral
                    (name : string)
                    (tryParse : (string -> bool * 'T))
                    (min : unit -> 'T)
                    (max : unit -> 'T)
                    (conv : float -> 'T) : Decoder< 'T > =

        fun path value ->
            if Helpers.isNumber value then
                if Helpers.isIntegralValue value then
                    let fValue = Helpers.asFloat value
                    if (float(min())) <= fValue && fValue <= (float(max())) then
                        Ok(conv fValue)
                    else
                        (path, BadPrimitiveExtra(name, value, "Value was either too large or too small for " + name)) |> Error
                else
                    (path, BadPrimitiveExtra(name, value, "Value is not an integral value")) |> Error
            elif Helpers.isString value then
                match tryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive(name, value)) |> Error
            else
                (path, BadPrimitive(name, value)) |> Error

    let sbyte : Decoder<sbyte> =
        integral
            "a sbyte"
            System.SByte.TryParse
            (fun () -> System.SByte.MinValue)
            (fun () -> System.SByte.MaxValue)
            sbyte

    /// Alias to Decode.uint8
    let byte : Decoder<byte> =
        integral
            "a byte"
            System.Byte.TryParse
            (fun () -> System.Byte.MinValue)
            (fun () -> System.Byte.MaxValue)
            byte

    let int16 : Decoder<int16> =
        integral
            "an int16"
            System.Int16.TryParse
            (fun () -> System.Int16.MinValue)
            (fun () -> System.Int16.MaxValue)
            int16

    let uint16 : Decoder<uint16> =
        integral
            "an uint16"
            System.UInt16.TryParse
            (fun () -> System.UInt16.MinValue)
            (fun () -> System.UInt16.MaxValue)
            uint16

    let int : Decoder<int> =
        integral
            "an int"
            System.Int32.TryParse
            (fun () -> System.Int32.MinValue)
            (fun () -> System.Int32.MaxValue)
            int

    let uint32 : Decoder<uint32> =
        integral
            "an uint32"
            System.UInt32.TryParse
            (fun () -> System.UInt32.MinValue)
            (fun () -> System.UInt32.MaxValue)
            uint32

    let int64 : Decoder<int64> =
        integral
            "an int64"
            System.Int64.TryParse
            (fun () -> System.Int64.MinValue)
            (fun () -> System.Int64.MaxValue)
            int64

    let uint64 : Decoder<uint64> =
        integral
            "an uint64"
            System.UInt64.TryParse
            (fun () -> System.UInt64.MinValue)
            (fun () -> System.UInt64.MaxValue)
            uint64

    let bigint : Decoder<bigint> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asInt token |> bigint |> Ok
            elif Helpers.isString token then
                match bigint.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a bigint", token)) |> Error
            else
                (path, BadPrimitive("a bigint", token)) |> Error

    let bool : Decoder<bool> =
        fun path token ->
            if Helpers.isBool token then
                Ok(Helpers.asBool token)
            else
                (path, BadPrimitive("a boolean", token)) |> Error

    let float : Decoder<float> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asFloat token |> Ok
            else
                (path, BadPrimitive("a float", token)) |> Error

    let float32 : Decoder<float32> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asFloat32 token |> Ok
            else
                (path, BadPrimitive("a float", token)) |> Error

    let decimal : Decoder<decimal> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asDecimal token |> Ok
            elif Helpers.isString token then
                match System.Decimal.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a decimal", token)) |> Error
            else
                (path, BadPrimitive("a decimal", token)) |> Error

    [<System.Obsolete("Please use datetimeUtc instead.")>]
    let datetime : Decoder<System.DateTime> =
        fun path token ->
            if Helpers.isDate token then
                token.AsDateTime.ToUniversalTime() |> Ok
            elif Helpers.isString token then
                match System.DateTime.TryParse (Helpers.asString token, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, x -> x.ToUniversalTime() |> Ok
                | _ -> (path, BadPrimitive("a datetime", token)) |> Error
            else
                (path, BadPrimitive("a datetime", token)) |> Error

    /// Decode a System.DateTime value using Sytem.DateTime.TryParse, then convert it to UTC.
    let datetimeUtc : Decoder<System.DateTime> =
        fun path token ->
            if Helpers.isDate token then
                token.AsDateTime.ToUniversalTime() |> Ok
            else if Helpers.isString token then
                match System.DateTime.TryParse (Helpers.asString token) with
                | true, x -> x.ToUniversalTime() |> Ok
                | _ -> (path, BadPrimitive("a datetime", token)) |> Error
            else
                (path, BadPrimitive("a datetime", token)) |> Error

    /// Decode a System.DateTime with DateTime.TryParse; uses default System.DateTimeStyles.
    let datetimeLocal : Decoder<System.DateTime> =
        fun path token ->
            if Helpers.isDate token then
                token.AsDateTime |> Ok
            else if Helpers.isString token then
                match System.DateTime.TryParse (Helpers.asString token) with
                | true, x -> x |> Ok
                | _ -> (path, BadPrimitive("a datetime", token)) |> Error
            else
                (path, BadPrimitive("a datetime", token)) |> Error

    let datetimeOffset : Decoder<System.DateTimeOffset> =
        fun path token ->
            if Helpers.isDate token then
                token.AsDateTime |> System.DateTimeOffset |> Ok
            elif Helpers.isString token then
                match System.DateTimeOffset.TryParse (Helpers.asString token, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a datetimeoffset", token)) |> Error
            else
                (path, BadPrimitive("a datetimeoffset", token)) |> Error

    let timespan : Decoder<System.TimeSpan> =
        fun path token ->
            if token.Type = JTokenType.String then
                match System.TimeSpan.TryParse (Helpers.asString token, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a timespan", token)) |> Error
            else
                (path, BadPrimitive("a timespan", token)) |> Error

    /////////////////////////
    // Object primitives ///
    ///////////////////////

    let private decodeMaybeNull path (decoder : Decoder<'T>) value =
        // The decoder may be an option decoder so give it an opportunity to check null values

        // We catch the null value case first to avoid executing the decoder logic
        // Indeed, if the decoder logic try to access the value to do something with it,
        // it can throw an exception about the value being null
        if Helpers.isNullValue value then
            Ok None
        else
            match decoder path value with
            | Ok v -> Ok(Some v)
            | Error er -> Error er

    let optional (fieldName : string) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path value ->
            if Helpers.isObject value then
                let fieldValue = Helpers.getField fieldName value
                if Helpers.isUndefined fieldValue then Ok None
                else decodeMaybeNull (path + "." + fieldName) decoder fieldValue
            else
                Error(path, BadType("an object", value))

    let private badPathError fieldNames currentPath value =
        let currentPath = defaultArg currentPath ("$"::fieldNames |> String.concat ".")
        let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
        Error(currentPath, BadPath (msg, value, List.tryLast fieldNames |> Option.defaultValue ""))

    let optionalAt (fieldNames : string list) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun firstPath firstValue ->
            ((firstPath, firstValue, None), fieldNames)
            ||> List.fold (fun (curPath, curValue, res) field ->
                match res with
                | Some _ -> curPath, curValue, res
                | None ->
                    if Helpers.isNullValue curValue then
                        curPath, curValue, Some (Ok None)
                    elif Helpers.isObject curValue then
                        let curValue = Helpers.getField field curValue
                        curPath + "." + field, curValue, None
                    else
                        let res = Error(curPath, BadType("an object", curValue))
                        curPath, curValue, Some res)
            |> function
                | _, _, Some res -> res
                | lastPath, lastValue, None ->
                    if Helpers.isUndefined lastValue then Ok None
                    else decodeMaybeNull lastPath decoder lastValue

    let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path value ->
            if Helpers.isObject value then
                let fieldValue = Helpers.getField fieldName value
                if Helpers.isUndefined fieldValue then
                    Error(path, BadField ("an object with a field named `" + fieldName + "`", value))
                else
                    decoder (path + "." + fieldName) fieldValue
            else
                Error(path, BadType("an object", value))

    let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
        fun firstPath firstValue ->
            ((firstPath, firstValue, None), fieldNames)
            ||> List.fold (fun (curPath, curValue, res) field ->
                match res with
                | Some _ -> curPath, curValue, res
                | None ->
                    if Helpers.isNullValue curValue then
                        let res = badPathError fieldNames (Some curPath) firstValue
                        curPath, curValue, Some res
                    elif Helpers.isObject curValue then
                        let curValue = Helpers.getField field curValue
                        if Helpers.isUndefined curValue then
                            let res = badPathError fieldNames None firstValue
                            curPath, curValue, Some res
                        else
                            curPath + "." + field, curValue, None
                    else
                        let res = Error(curPath, BadType("an object", curValue))
                        curPath, curValue, Some res)
            |> function
                | _, _, Some res -> res
                | lastPath, lastValue, None ->
                    decoder lastPath lastValue

    let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path token ->
            let currentPath = path + ".[" + (Operators.string requestedIndex) + "]"
            if Helpers.isArray token then
                let vArray = Helpers.asArray token
                if requestedIndex < vArray.Length then
                    decoder currentPath (vArray.[requestedIndex])
                else
                    let msg =
                        "a longer array. Need index `"
                            + (requestedIndex.ToString())
                            + "` but there are only `"
                            + (vArray.Length.ToString())
                            + "` entries"

                    (currentPath, TooSmallArray(msg, token))
                    |> Error
            else
                (currentPath, BadPrimitive("an array", token))
                |> Error

    let option (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path value ->
            if Helpers.isNullValue value then Ok None
            else decoder path value |> Result.map Some

    //////////////////////
    // Data structure ///
    ////////////////////

    let list (decoder : Decoder<'value>) : Decoder<'value list> =
        fun path value ->
            if Helpers.isArray value then
                let mutable i = -1
                let tokens = Helpers.asArray value
                (Ok [], tokens) ||> Array.fold (fun acc value ->
                    i <- i + 1
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match decoder (path + ".[" + (i.ToString()) + "]") value with
                        | Error er -> Error er
                        | Ok value -> Ok (value::acc))
                |> Result.map List.rev
            else
                (path, BadPrimitive ("a list", value))
                |> Error

    let array (decoder : Decoder<'value>) : Decoder<'value array> =
        fun path value ->
            if Helpers.isArray value then
                let mutable i = -1
                let tokens = Helpers.asArray value
                let arr = Array.zeroCreate tokens.Length
                (Ok arr, tokens) ||> Array.fold (fun acc value ->
                    i <- i + 1
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match decoder (path + ".[" + (i.ToString()) + "]") value with
                        | Error er -> Error er
                        | Ok value -> acc.[i] <- value; Ok acc)
            else
                (path, BadPrimitive ("an array", value))
                |> Error

    let keys : Decoder<string list> =
        fun path value ->
            if Helpers.isObject value then
                let value = value.AsDocument
                value.Keys
                |> List.ofSeq |> Ok
            else
                (path, BadPrimitive ("an object", value))
                |> Error


    let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
        fun path value ->
            match keys path value with
            | Ok objecKeys ->
                (Ok [], objecKeys ) ||> Seq.fold (fun acc prop ->
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match Helpers.getField prop value |> decoder path with
                        | Error er -> Error er
                        | Ok value -> (prop, value)::acc |> Ok)
                |> Result.map List.rev
            | Error e -> Error e

    //////////////////////////////
    // Inconsistent Structure ///
    ////////////////////////////

    let oneOf (decoders : Decoder<'value> list) : Decoder<'value> =
        fun path value ->
            let rec runner (decoders : Decoder<'value> list) (errors : string list) =
                match decoders with
                | head::tail ->
                    match fromValue path head value with
                    | Ok v ->
                        Ok v
                    | Error error -> runner tail (errors @ [error])
                | [] -> (path, BadOneOf errors) |> Error

            runner decoders []

    //////////////////////
    // Fancy decoding ///
    ////////////////////

    let nil (output : 'a) : Decoder<'a> =
        fun path token ->
            if Helpers.isNullValue token then
                Ok output
            else
                (path, BadPrimitive("null", token)) |> Error

    let value _ v = Ok v

    let succeed (output : 'a) : Decoder<'a> =
        fun _ _ ->
            Ok output

    let fail (msg: string) : Decoder<'a> =
        fun path _ ->
            (path, FailMessage msg) |> Error

    let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
        fun path value ->
            match decoder path value with
            | Error error -> Error error
            | Ok result -> cb result path value

    let all (decoders: Decoder<'a> list): Decoder<'a list> =
        fun path value ->
            let rec runner (decoders: Decoder<'a> list) (values: 'a list) =
                match decoders with
                | decoder :: tail ->
                    match decoder path value with
                    | Ok value -> runner tail (values @ [ value ])
                    | Error error -> Error error
                | [] -> Ok values

            runner decoders []

    /////////////////////
    // Map functions ///
    ///////////////////

    let map
        (ctor : 'a -> 'value)
        (d1 : Decoder<'a>) : Decoder<'value> =
        fun path value ->
            match d1 path value with
            | Ok v1 -> Ok (ctor v1)
            | Error er -> Error er

    let map2
        (ctor : 'a -> 'b -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value with
            | Ok v1, Ok v2 -> Ok (ctor v1 v2)
            | Error er,_ -> Error er
            | _,Error er -> Error er

    let map3
        (ctor : 'a -> 'b -> 'c -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value with
            | Ok v1, Ok v2, Ok v3 -> Ok (ctor v1 v2 v3)
            | Error er,_,_ -> Error er
            | _,Error er,_ -> Error er
            | _,_,Error er -> Error er

    let map4
        (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (ctor v1 v2 v3 v4)
            | Error er,_,_,_ -> Error er
            | _,Error er,_,_ -> Error er
            | _,_,Error er,_ -> Error er
            | _,_,_,Error er -> Error er

    let map5
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5 -> Ok (ctor v1 v2 v3 v4 v5)
            | Error er,_,_,_,_ -> Error er
            | _,Error er,_,_,_ -> Error er
            | _,_,Error er,_,_ -> Error er
            | _,_,_,Error er,_ -> Error er
            | _,_,_,_,Error er -> Error er

    let map6
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6 -> Ok (ctor v1 v2 v3 v4 v5 v6)
            | Error er,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_ -> Error er
            | _,_,_,Error er,_,_ -> Error er
            | _,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,Error er -> Error er

    let map7
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>)
        (d7 : Decoder<'g>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7)
            | Error er,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,Error er -> Error er

    let map8
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>)
        (d7 : Decoder<'g>)
        (d8 : Decoder<'h>) : Decoder<'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value, d8 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7, Ok v8 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
            | Error er,_,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,_,Error er -> Error er

    //////////////////////
    // Object builder ///
    ////////////////////

    /// <summary>
    /// Allow to incrementally apply a decoder, for building large objects.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// type Point =
    ///     {
    ///         X : float
    ///         Y : float
    ///     }
    ///
    /// module Point =
    ///     let create x y = { X = x; Y = y }
    ///
    ///     let decode =
    ///         Decode.succeed create
    ///             |> Decode.andMap (Decode.field "x" Decode.float)
    ///             |> Decode.andMap (Decode.field "y" Decode.float)
    /// </code>
    /// </example>
    let andMap<'a, 'b> : 'a Decoder -> ('a -> 'b) Decoder -> 'b Decoder =
        map2 (|>)

    type IRequiredGetter =
        abstract Field : string -> Decoder<'a> -> 'a
        abstract At : List<string> -> Decoder<'a> -> 'a
        abstract Raw : Decoder<'a> -> 'a

    type IOptionalGetter =
        abstract Field : string -> Decoder<'a> -> 'a option
        abstract At : List<string> -> Decoder<'a> -> 'a option
        abstract Raw : Decoder<'a> -> 'a option

    type IGetters =
        abstract Required: IRequiredGetter
        abstract Optional: IOptionalGetter

    let private unwrapWith (errors: ResizeArray<DecoderError>) path (decoder: Decoder<'T>) value: 'T =
        match decoder path value with
        | Ok v -> v
        | Error er -> errors.Add(er); Unchecked.defaultof<'T>

    type Getters<'T>(path: string, v: JsonValue) =
        let mutable errors = ResizeArray<DecoderError>()
        let required =
            { new IRequiredGetter with
                member __.Field (fieldName : string) (decoder : Decoder<_>) =
                    unwrapWith errors path (field fieldName decoder) v
                member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                    unwrapWith errors path (at fieldNames decoder) v
                member __.Raw (decoder: Decoder<_>) =
                    unwrapWith errors path decoder v }
        let optional =
            { new IOptionalGetter with
                member __.Field (fieldName : string) (decoder : Decoder<_>) =
                    unwrapWith errors path (optional fieldName decoder) v
                member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                    unwrapWith errors path (optionalAt fieldNames decoder) v
                member __.Raw (decoder: Decoder<_>) =
                    match decoder path v with
                    | Ok v -> Some v
                    | Error((_, reason) as error) ->
                        match reason with
                        | BadPrimitive(_,v)
                        | BadPrimitiveExtra(_,v,_)
                        | BadType(_,v) ->
                            if Helpers.isNullValue v then None
                            else errors.Add(error); Unchecked.defaultof<_>
                        | BadField _
                        | BadPath _ -> None
                        | TooSmallArray _
                        | FailMessage _
                        | BadOneOf _ -> errors.Add(error); Unchecked.defaultof<_> }
        member __.Errors: _ list = Seq.toList errors
        interface IGetters with
            member __.Required = required
            member __.Optional = optional

    let object (builder: IGetters -> 'value) : Decoder<'value> =
        fun path v ->
            let getters = Getters(path, v)
            let result = builder getters
            match getters.Errors with
            | [] -> Ok result
            | fst::_ as errors ->
                if errors.Length > 1 then
                    let errors = List.map errorToString errors
                    (path, BadOneOf errors) |> Error
                else
                    Error fst

    ///////////////////////
    // Tuples decoders ///
    ////////////////////

    let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                succeed (v1, v2)
            )
        )

    let tuple3 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>) : Decoder<'T1 * 'T2 * 'T3> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    succeed (v1, v2, v3)
                )
            )
        )

    let tuple4 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>) : Decoder<'T1 * 'T2 * 'T3 * 'T4> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        succeed (v1, v2, v3, v4)
                    )
                )
            )
        )

    let tuple5 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            succeed (v1, v2, v3, v4, v5)
                        )
                    )
                )
            )
        )

    let tuple6 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                succeed (v1, v2, v3, v4, v5, v6)
                            )
                        )
                    )
                )
            )
        )

    let tuple7 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    succeed (v1, v2, v3, v4, v5, v6, v7)
                                )
                            )
                        )
                    )
                )
            )
        )

    let tuple8 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>)
               (decoder8: Decoder<'T8>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    index 7 decoder8
                                    |> andThen (fun v8 ->
                                        succeed (v1, v2, v3, v4, v5, v6, v7, v8)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    ///////////
    // Map ///
    /////////

    let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
        map Map.ofList (keyValuePairs decoder)

    let map' (keyDecoder : Decoder<'key>) (valueDecoder : Decoder<'value>) : Decoder<Map<'key, 'value>> =
        map Map.ofSeq (array (tuple2 keyDecoder valueDecoder))

    ////////////
    // Enum ///
    /////////

    module Enum =

        let byte<'TEnum when 'TEnum : enum<byte>> : Decoder<'TEnum> =
            byte
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<byte, 'TEnum> value
                |> succeed
            )

        let sbyte<'TEnum when 'TEnum : enum<sbyte>> : Decoder<'TEnum> =
            sbyte
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<sbyte, 'TEnum> value
                |> succeed
            )

        let int16<'TEnum when 'TEnum : enum<int16>> : Decoder<'TEnum> =
            int16
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<int16, 'TEnum> value
                |> succeed
            )

        let uint16<'TEnum when 'TEnum : enum<uint16>> : Decoder<'TEnum> =
            uint16
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<uint16, 'TEnum> value
                |> succeed
            )

        let int<'TEnum when 'TEnum : enum<int>> : Decoder<'TEnum> =
            int
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<int, 'TEnum> value
                |> succeed
            )

        let uint32<'TEnum when 'TEnum : enum<uint32>> : Decoder<'TEnum> =
            uint32
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<uint32, 'TEnum> value
                |> succeed
            )
