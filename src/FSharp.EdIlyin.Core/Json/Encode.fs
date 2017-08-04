module FSharp.EdIlyin.Core.Json.Encode

open Chiron


let encode = Json.format


let string = Json.String


let int = decimal >> Json.Number


let uint32 (x: uint32) = decimal x |> Json.Number


let float (x: float) = decimal x |> Json.Number


let bool = Json.Bool


let Null = Json.Null


let list = Json.Array


let object = Map.ofList >> Json.Object
