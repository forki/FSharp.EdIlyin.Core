module FSharp.EdIlyin.Core.Json.Decode

open Chiron
open FSharp.EdIlyin.Core


let value : Decode.Decoder<Json, Json> =
    Decode.primitive "an JSON Value" Decode.decoded


let decodeValue decoder json = Decode.decode decoder json


let decodeString decoder json =
    Json.tryParse json
        |> Result.fromChoice
        |> Result.andThen (decodeValue decoder)


let primitive<'T> label func =
    Decode.primitive label <|
        fun value ->
            try
                match func value with
                    | Some (o: obj) ->
                        match o with
                            | :? 'T as x -> Decode.decoded x
                            | _ -> Decode.expectingButGot label value

                    | None -> Decode.expectingButGot label value

            with | e -> Operators.string e |> Decode.errorMessage



let uint16 =
    function
        | Number x -> box x |> Some
        | _ -> None
        |> primitive<uint16> "an UInt16"


let string =
    function
        | String x -> match x with | null -> None | s -> box s |> Some
        | _ -> None
        |> primitive<string> "a String"


let field fieldName decoder =
    let label =
        sprintf "%s field '%s'" (Decode.getLabel decoder) fieldName

    let got unexpected = Decode.expectingButGot label unexpected

    function
        | Object map ->
            match Map.tryFind fieldName map with
                | None -> got map
                | Some x -> Decode.run decoder x

        | other -> got other
        |> Decode.primitive label


let list decoder =
    let itemLabel = Decode.getLabel decoder
    let label = sprintf "%s list" itemLabel
    let got unexpected = Decode.expectingButGot label unexpected

    Decode.andThen
        (function
            | Array list ->
                List.map
                    (Decode.run decoder
                        >> Decode.fromDecodeResult itemLabel
                    )
                    list
                    |> Decode.combineList

            | other -> got other |> Decode.fromDecodeResult label
        )
        value


let uint32 =
    function
        | Number x -> box x |> Some
        | _ -> None
        |> primitive<uint32> "an UInt32"


let int =
    function
        | Number x -> box x |> Some
        | _ -> None
        |> primitive<int> "an Int"
