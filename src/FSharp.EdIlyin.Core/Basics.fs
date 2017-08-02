namespace FSharp.EdIlyin.Core

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
[<AutoOpen>]
module Basics =
    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    let (=>) x y = x, y


    let tuple x y = x, y


    let curry f x y = f (x, y)


    let first x _ = x


    let second _ x = x


    let flip func x y = func y x
