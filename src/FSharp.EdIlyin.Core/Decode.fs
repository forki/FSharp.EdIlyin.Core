module FSharp.EdIlyin.Core.Decode


type Label = string


type ErrorMessage = string


type DecodeResult<'T> =
    | Decoded of 'T
    | ExpectingButGot of Label * string
    | ErrorMessage of ErrorMessage


type Decoder<'From,'To> = {
    decoder: 'From -> DecodeResult<'To>
    label: Label
}


let run decoder value = decoder.decoder value


let getLabel decoder = decoder.label


let decode decoder source =
    run decoder source
        |> function
            | Decoded value -> Ok value

            | ExpectingButGot (expecting, got) ->
                sprintf "Expecting %s, but instead got: %A"
                    expecting
                    got
                    |> Error

            | ErrorMessage message -> Error message


let inline primitive label func = { decoder = func; label = label }


let fail error = primitive "anything" <| fun _ -> ErrorMessage error


let succeed value = primitive "anything" <| fun _ -> Decoded value


let setLabel label decoder = { decoder with label = label }


let (<?>) decoder label = setLabel label decoder


let andThen func decoder =
    let label = getLabel decoder

    primitive label
        (fun input ->
            match run decoder input with
                | Decoded value -> run (func value) input

                | ExpectingButGot (expecting, got) ->
                    expecting => got |> ExpectingButGot

                | ErrorMessage message -> ErrorMessage message
        )


let (>>=) decoder func = andThen func decoder


let andMap decoder functionDecoder =
    functionDecoder >>= (fun f -> decoder >>= (f >> succeed))


let (<*>) fnDecoder decoder = andMap decoder fnDecoder


let map func decoder =
    succeed func
        <*> decoder
        <?> sprintf "{ %s }" (getLabel decoder)


let map2 func decoder1 decoder2 =
    succeed func
        <*> decoder1
        <*> decoder2
        <?> sprintf "{ %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)


let map3 func decoder1 decoder2 decoder3 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <?> sprintf "{ %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)


let map4 func decoder1 decoder2 decoder3 decoder4 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <?> sprintf "{ %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)


let map5 func decoder1 decoder2 decoder3 decoder4 decoder5 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <?> sprintf "{ %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)


let map6 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <?> sprintf "{ %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)


let map7 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <*> decoder7
        <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)
            (getLabel decoder7)


let map8 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <*> decoder7
        <*> decoder8
        <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)
            (getLabel decoder7)
            (getLabel decoder8)


let map9 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <*> decoder7
        <*> decoder8
        <*> decoder9
        <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)
            (getLabel decoder7)
            (getLabel decoder8)
            (getLabel decoder9)


let fromResult result = Result.unpack fail succeed result


// /// Match an input token if the predicate is satisfied
let satisfy nextFn predicate label =
    let innerFn input =
        let remainingInput, opt = nextFn input

        match opt with
            | None ->
                let err = "No more input"
                ErrorMessage err

            | Some first ->
                match predicate first with
                    | Ok value ->
                        value => remainingInput |> ExpectingButGot

                    | Error unexpected ->
                        label
                            => unexpected
                            |> ExpectingButGot

    // return the parser
    {decoder=innerFn;label=label}


/// Run the parser on a InputState
// let runOnInput parser input =
    // call inner function with input
    // parser.decoder input


// let parseAny parser input =
//      run parser input
//         |> Result.map fst


let tuple p1 p2 =
    let label = sprintf "%s and %s" (getLabel p1) (getLabel p2)
    p1 >>= (fun p1Result ->
    p2 >>= (fun p2Result ->
        succeed (p1Result,p2Result) ))
    <?> label


let (.>>.) = tuple


let (>>.) p1 p2 = p1 .>>. p2 |> map snd


let resultFromResult =
    function
        | Error x -> ErrorMessage x
        | Ok x -> Decoded x


let result decoder =
    let label = getLabel decoder
    decode decoder >> Decoded |> primitive label


let inline expectingButGot expecting got =
    expecting => sprintf "%A" got |> ExpectingButGot


let inline errorMessage errorMessage = ErrorMessage errorMessage


let inline decoded value = Decoded value


let fromDecodeResult label decodeResult =
    primitive label <| fun _ -> decodeResult


let combineList decoderList =
    succeed List.empty
        |> List.foldBack (map2 (fun e l -> e::l)) decoderList


let maybe decoder =
    decode decoder
        >> Result.unpack (fun _ -> decoded None) (Some >> decoded)
        |> primitive ""


let withDefault fallback decoder =
    maybe decoder |> map (Option.withDefault fallback)
