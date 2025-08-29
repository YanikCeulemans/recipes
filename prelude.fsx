#r "nuget: FsToolkit.ErrorHandling, 4.18.0,usepackagetargets=true"

let flip f a b = f b a
let always x _ = x

module List =
    let cons x xs = x :: xs
    let appendWith x xs = xs @ [ x ]

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

    let prefix prefix (str: string) = prefix + str

    let replace (target: string) (replacement: string) (text: string) =
        text.Replace(target, replacement)

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
        open Operators

        let sequenceA (xs: Result<'a, string> seq) : Result<'a seq, string> =
            let go curr acc = flip List.cons <!> acc <*> curr

            Seq.foldBack go xs (Ok []) |> Result.map seq

module Path =
    open System.IO

    let modifyFileName f (fullPath: string) =
        let dir, fileName, ext =
            Path.GetDirectoryName fullPath,
            Path.GetFileNameWithoutExtension fullPath,
            Path.GetExtension fullPath

        Path.Combine(dir, f fileName + ext)

    let modifyExt (buildExt: string -> string) (fileName: string) =
        let dir = Path.GetDirectoryName fileName
        let extlessPath = Path.GetFileNameWithoutExtension fileName
        let ext = Path.GetExtension fileName

        let newExt = (buildExt ext).TrimStart '.'
        Path.Combine(dir, $"%s{extlessPath}.%s{newExt}")

[<AutoOpen>]
module ActivePatterns =
    open System

    let (|Uri|_|) candidate =
        try
            Some(System.Uri candidate)
        with _ ->
            None

    let (|FileUri|_|) =
        function
        | Uri uri when uri.IsFile -> Some uri
        | _ -> None

    let (|ExternalUri|_|) =
        function
        | Uri uri when not uri.IsFile -> Some uri
        | _ -> None

module Validation =
    open FsToolkit.ErrorHandling

    let traverse
        (f: 'a -> Validation<'b, 'e>)
        (xs: 'a seq)
        : Validation<'b seq, 'e> =
        let go (acc: Validation<'b list, 'e>) (curr: 'a) =
            f curr
            |> Validation.map List.cons
            |> fun vf -> Validation.apply vf acc

        xs
        |> Seq.fold go (Validation.ok [])
        |> Validation.map List.rev
        |> Validation.map seq
