module Prelude =
    let flip f a b = f b a

module List =
    let cons x xs = x :: xs

module Tuple =
    let create a b = a, b

module String =
    let braced (str: string) = $"(%s{str})"

module Result =
    let ap
        (ma: Result<'a, string>)
        (mf: Result<'a -> 'b, string>)
        : Result<'b, string> =
        match mf, ma with
        | Error errF, Error errA -> Error $"{errF}; {errA}"
        | Error err, _
        | _, Error err -> Error err
        | Ok f, Ok a -> Ok(f a)

    let requireSome (reason: string) (ma: 'a option) : Result<'a, string> =
        match ma with
        | None -> Error reason
        | Some a -> Ok a

    module Operators =
        let (<!>) f ma = Result.map f ma

        let (<*>) mf ma = ap ma mf

    module Traversable =
        open Prelude
        open Operators

        let sequenceA (xs: Result<'a, string> seq) : Result<'a seq, string> =
            let go curr acc = flip List.cons <!> acc <*> curr

            Seq.foldBack go xs (Ok []) |> Result.map seq
