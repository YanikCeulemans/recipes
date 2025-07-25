#r "../_lib/Fornax.Core.dll"
#r "nuget: Kadlet"

open System.IO
open Kadlet

module Prelude =
    let flip f a b = f b a

module List =
    let cons x xs = x :: xs

module Tuple =
    let create a b = a, b

module Result =
    let ap
        (mf: Result<'a -> 'b, string>)
        (ma: Result<'a, string>)
        : Result<'b, string> =
        match mf, ma with
        | Error errF, Error errA -> Error $"{errF}; {errA}"
        | Error err, _
        | _, Error err -> Error err
        | Ok f, Ok a -> Ok(f a)

[<AutoOpen>]
module ResultOperators =
    let (<!>) f ma = Result.map f ma

    let (<*>) mf ma = Result.ap mf ma

module ResultTraversable =
    open Prelude

    let sequenceA (xs: Result<'a, string> seq) : Result<'a seq, string> =
        let go curr acc = flip List.cons <!> acc <*> curr

        Seq.foldBack go xs (Ok []) |> Result.map seq

module ResultValidation =
    let requireSingle (reason: 'e) (m: Result<'a array, 'e>) = failwith "todo"


type Ingredient = {
    Name: string
    Amount: int
    Unit: string
    Variant: string
}

type Ingredients = {
    Serving: int
    Ingredients: Set<Ingredient>
}

type Recipe = {
    Name: string
    Tags: string array option
    KeyInfo: Map<string, string> option
    Ingredients: Ingredients
    Instructions: string array
}

type KdlParser<'a> = KdlDocument -> Result<'a, string>

module KdlNodesParser =
    let runParser (parser: KdlParser<'a>) doc : Result<'a, string> = parser doc

    let rtn (x: 'a) : KdlParser<'a> = fun _ -> Ok x

    let fail (e: string) : KdlParser<'a> = fun _ -> Error e

    let map (f: 'a -> 'b) (ax: KdlParser<'a>) : KdlParser<'b> =
        fun doc ->
            let a = runParser ax doc
            Result.map f a

    let alt (parserB: KdlParser<'a>) (parserA: KdlParser<'a>) : KdlParser<'a> =
        fun doc ->
            match runParser parserA doc with
            | Ok a -> Ok a
            | Error errA ->
                match runParser parserB doc with
                | Ok b -> Ok b
                | Error errB -> Error $"{errA}; {errB}"

    let requireSingle
        (reason: string)
        (parser: KdlParser<'a array>)
        : KdlParser<'a> =
        fun doc ->
            runParser parser doc
            |> Result.bind (fun xs ->
                match xs with
                | [| x |] -> Ok x
                | _ -> Error reason
            )

    let opt (parser: KdlParser<'a>) : KdlParser<'a option> =
        parser |> map Some |> alt (rtn None)

    let private firstNodeNamed (name: string) (nodes: KdlNode seq) =
        nodes |> Seq.tryFind (fun n -> n.Identifier = name)

    module ArgParsers =
        let str (v: KdlValue) : Result<string, string> =
            match v with
            | :? KdlString as s -> Ok s.Value
            | _ -> Error "expected a kdl string value, instead got: %A{v}"

    module NodeParsers =
        let node (name: string) (parser: KdlParser<'a>) : KdlParser<'a> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n -> runParser parser n.Children

        let args
            (name: string)
            (argParser: KdlValue -> Result<'a, string>)
            : KdlParser<'a array> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n ->
                    n.Arguments
                    |> Seq.map argParser
                    |> ResultTraversable.sequenceA
                    |> Result.map Array.ofSeq

    // let str: KdlNodesParser<string> = failwith "todo"

    // let many (parser: KdlNodesParser<'a>) : KdlNodesParser<'a array> =
    //     failwith "todo"

    [<AutoOpen>]
    module ComputationExpression =
        type KdlNodesParserBuilder() =
            member _.BindReturn(ax, f) = map f ax

            member _.MergeSources
                (ma: KdlParser<'a>, mb: KdlParser<'b>)
                : KdlParser<'a * 'b> =
                fun nodes ->
                    let resA = runParser ma nodes
                    let resB = runParser mb nodes

                    Tuple.create <!> resA <*> resB


        let kdlNodesParser = KdlNodesParserBuilder()

let contentDir = "recipes"

let reader = KdlReader()

open KdlNodesParser

let loadFile (filePath: string) : Recipe =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let recipeParser: KdlParser<Recipe> =
        kdlNodesParser {
            let! name =
                NodeParsers.args "name" ArgParsers.str
                |> requireSingle "a recipe must only have a single name"

            and! tags = opt (NodeParsers.args "tags" ArgParsers.str)

            return {
                Name = name
                Tags = tags
                KeyInfo = None
                Ingredients = { Serving = 0; Ingredients = Set.empty }
                Instructions = [||]
            }
        }

    let p = NodeParsers.node "recipe" recipeParser

    match runParser p doc with
    | Ok r -> r
    | Error reason -> failwith $"could not parse: %s{reason}"


let loader (projectRoot: string) (siteContent: SiteContents) =
    let recipesPath = Path.Combine(projectRoot, contentDir)
    let options = EnumerationOptions(RecurseSubdirectories = true)
    let files = Directory.GetFiles(recipesPath, "*", options)

    files
    |> Array.filter (fun s -> s.EndsWith ".kdl")
    |> Array.map loadFile
    |> Array.iter siteContent.Add

    siteContent
