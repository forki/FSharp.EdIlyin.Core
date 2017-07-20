module FSharp.EdIlyin.Core.Json.Decode

open Chiron
open FSharp.EdIlyin.Core


let string =
    let label = "a String"
    Decode.primitive label
        (fun value ->
            match value with
                | String str ->
                    match str with
                        | null -> label => value |> Decode.ExpectingButGot
                        | s -> Decode.Decoded s

                | _ -> label => value |> Decode.ExpectingButGot
        )


let value : Decode.Decoder<Json, Json> =
    Decode.primitive "an JSON Value" Decode.Decoded


let decodeValue decoder json = Decode.decode decoder json


let decodeString decoder json =
    Json.tryParse json
        |> Result.fromChoice
        |> Result.andThen (decodeValue decoder)