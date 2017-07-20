namespace FSharp.EdIlyin.Core

open Hopac


type Boxcar<'a> = Job<Result<'a,string>>


type Train<'a> = Job<seq<Result<'a,string>>>


module Boxcar =
    let unpack errFunc okFunc (boxcar: Boxcar<_>) =
        Job.map (Result.unpack errFunc okFunc) boxcar


    let bind (func: 'a -> Boxcar<'b>) (boxcar: Boxcar<'a>) : Boxcar<'b> =
        Job.bind (Result.unpack (Error >> Job.result) func) boxcar


    let result x : Boxcar<_> = Result.Ok x |> Job.result


    type BoxcarBuilder () =
        member this.Bind(m, f) = bind f m
        member this.Bind(m, f) = Job.fromAsync m |> bind f
        member this.Return(x) = result x
        member this.ReturnFrom(m) = m


    let fromResult result : Boxcar<_> = Job.result result


    let fromOption error option =
        Result.fromOption error option |> fromResult

    let andThen = bind


    let catch boxcar =
        Job.catch boxcar
            |> Job.map
                (Result.fromChoice
                    >> Result.mapError (fun e -> e.Message)
                )


    let conCollect boxcar =
        boxcar |> Job.conCollect |> Job.map Result.combine


    let seqCollect boxcar =
        boxcar |> Job.seqCollect |> Job.map Result.combine


    let map func boxcar =
        boxcar |> Job.map (Result.map func)


    let mapError func boxcar =
        boxcar |> Job.map (Result.mapError func)


[<AutoOpen>]
module BoxcarAutoOpen =
    let boxcar = Boxcar.BoxcarBuilder ()