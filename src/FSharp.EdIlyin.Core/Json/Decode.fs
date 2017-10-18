module FSharp.EdIlyin.Core.Json.Decode

open Chiron
open FSharp.EdIlyin.Core


let value : Decode.Decoder<Json, Json> =
    Decode.primitive "an JSON Value" Ok


let decodeValue decoder json = Decode.decode decoder json


let decodeString decoder json =
    match Json.tryParse json with
        | Choice1Of2 x -> Ok x
        | Choice2Of2 e -> Result.Error e
        |> Result.andThen (decodeValue decoder)


let primitive label func =
    Decode.primitive label <|
        fun value ->
            try
                match func value with
                    | Some x -> Ok x
                    | None -> Decode.expectingButGot label value

            with | e -> Operators.string e |> Decode.errorMessage



let uint16 =
    function
        | Number x -> uint16 x |> Some
        | _ -> None
        |> primitive "an UInt16"


let string =
    function
        | String null -> None
        | String x -> Some x
        | _ -> None
        |> primitive "a String"


let dict decoder =
    let label =
        Decode.getLabel decoder |> sprintf "maping of string to %s"

    function
        | Object o ->
            Map.toList o
                |> List.map
                    (fun (name, json) ->
                        decodeValue decoder json
                            |> Result.map (tuple name)
                    )
                |> Result.combineList
                |> Result.map Map.ofList
                |> Decode.resultFromResult

        | other -> Decode.expectingButGot label other
        |> Decode.primitive label


let field fieldName decoder =
    let label =
        sprintf "%s field '%s'" (Decode.getLabel decoder) fieldName

    let got unexpected = Decode.expectingButGot label unexpected

    function
        | Object o ->
            match Map.tryFind fieldName o with
                | None -> got o
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
                    (Decode.run decoder >> Decode.fromDecodeResult)
                    list
                    |> Decode.combineList

            | other -> got other |> Decode.fromDecodeResult
        )
        value


let uint32 =
    function
        | Number x -> uint32 x |> Some
        | _ -> None
        |> primitive "an UInt32"


let int =
    function
        | Number x -> int x |> Some
        | _ -> None
        |> primitive "an Int"


let at path decoder = List.foldBack field path decoder


let optionalField fieldName decoder =
    let finishDecoding json =
        match decodeValue (field fieldName value) json with
            | Ok _ -> field fieldName decoder |> Decode.map Some
            | Result.Error _ -> Decode.succeed None

    in value |> Decode.andThen finishDecoding


let bool =
    function
        | Bool x -> Some x
        | _ -> None
        |> primitive "a Boolean"


let float =
    function
        | Number x -> float x |> Some
        | _ -> None
        |> primitive "a Float"


let Null a =
    function
        | Null () -> Some a
        | _ -> None
        |> primitive "a Null"


let keyValuePairs decoder =
    let label = "a Key Value Pairs"

    function
        | Object map ->
            map
                |> Map.toList
                |> List.map
                    (fun (k, v) ->
                        Decode.run decoder v
                            |> Result.map (tuple k)
                    )
                |> Result.combineList

        | got -> Decode.expectingButGot label got

    |> Decode.primitive label
