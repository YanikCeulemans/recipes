module Prelude =
    let flip f a b = f b a
    let always x _ = x

module List =
    let cons x xs = x :: xs

module Tuple =
    let create a b = a, b

module String =
    let braced (str: string) = $"(%s{str})"
    let appendWith (str2: string) (str1: string) = str1 + str2

    let captilize (str: string) =
        match str with
        | null
        | "" -> str
        | _ -> System.Char.ToUpper str[0] |> string |> appendWith str[1..]

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

module File =
    open System.IO

    let replaceExt (buildExt: string -> string) (fileName: string) =
        let ext = Path.GetExtension fileName
        let extlessPath = Path.GetFileNameWithoutExtension fileName
        let newExt = (buildExt ext).TrimStart '.'
        $"%s{extlessPath}.%s{newExt}"
