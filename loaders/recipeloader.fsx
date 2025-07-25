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

[<AutoOpen>]
module ResultOperators =
    let (<!>) f ma = Result.map f ma

    let (<*>) mf ma = Result.ap ma mf

module ResultTraversable =
    open Prelude

    let sequenceA (xs: Result<'a, string> seq) : Result<'a seq, string> =
        let go curr acc = flip List.cons <!> acc <*> curr

        Seq.foldBack go xs (Ok []) |> Result.map seq

type IngredientUnit =
    | Pieces
    | OtherIngredientUnit of string

module IngredientUnit =
    let format =
        function
        | Pieces -> "pieces"
        | OtherIngredientUnit o -> o

type Ingredient = {
    Name: string
    Amount: int
    /// The default unit is pieces
    Unit: IngredientUnit
    /// The default is no variant
    Variant: string option
}

module Ingredient =
    let create name amount theUnit variant = {
        Name = name
        Amount = amount
        Unit = theUnit
        Variant = variant
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

module KdlParser =
    let runParser (parser: KdlParser<'a>) doc : Result<'a, string> = parser doc

    let rtn (x: 'a) : KdlParser<'a> = fun _ -> Ok x

    let fail (e: string) : KdlParser<'a> = fun _ -> Error e

    let map (f: 'a -> 'b) (ax: KdlParser<'a>) : KdlParser<'b> =
        fun doc ->
            let a = runParser ax doc
            Result.map f a

    let andThen
        (f: 'a -> Result<'b, string>)
        (ma: KdlParser<'a>)
        : KdlParser<'b> =
        fun doc ->
            let a = runParser ma doc
            Result.bind f a

    let alt (parserB: KdlParser<'a>) (parserA: KdlParser<'a>) : KdlParser<'a> =
        fun doc ->
            match runParser parserA doc with
            | Ok a -> Ok a
            | Error errA ->
                match runParser parserB doc with
                | Ok b -> Ok b
                | Error errB -> Error $"{errA}; {errB}"

    let opt (parser: KdlParser<'a>) : KdlParser<'a option> =
        parser |> map Some |> alt (rtn None)

    let ofResult (result: Result<'a, string>) : KdlParser<'a> =
        fun _doc -> result

    let ap
        (parserA: KdlParser<'a>)
        (parserF: KdlParser<'a -> 'b>)
        : KdlParser<'b> =
        fun doc ->
            let resA = runParser parserA doc
            let resF = runParser parserF doc

            Result.ap resA resF


    let sequenceA (parsers: KdlParser<'a> array) : KdlParser<'a array> =
        let go curr acc = rtn List.cons |> ap curr |> ap acc

        Array.foldBack go parsers (rtn []) |> map Array.ofList

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

    let requireSome
        (reason: string)
        (parser: KdlParser<'a option>)
        : KdlParser<'a> =
        fun doc ->
            runParser parser doc
            |> Result.bind (fun xs ->
                match xs with
                | Some x -> Ok x
                | _ -> Error reason
            )

    let private firstNodeNamed (name: string) (nodes: KdlNode seq) =
        nodes |> Seq.tryFind (fun n -> n.Identifier = name)

    module ArgParsers =
        let nth
            (name: string)
            (index: int)
            (parser: KdlValue -> Result<'a, string>)
            (args: KdlValue array)
            : Result<'a, string> =
            Array.tryItem index args
            |> Result.requireSome
                $"missing '%s{name}' argument at index %d{index}"
            |> Result.bind parser

        let nthOpt
            (name: string)
            (index: int)
            (parser: KdlValue -> Result<'a, string>)
            (args: KdlValue array)
            : Result<'a option, string> =
            match Array.tryItem index args with
            | None -> Ok None
            | Some a ->
                parser a
                |> Result.mapError (fun err ->
                    $"parsing error for arg '%s{name}' at index %d{index}: %s{err}"
                )
                |> Result.map Some

        let str (v: KdlValue) : Result<string, string> =
            match v with
            | :? KdlString as s -> Ok s.Value
            | _ -> Error "expected a kdl string value, instead got: %A{v}"

        let int (v: KdlValue) : Result<int, string> =
            match v with
            | :? KdlInt32 as s -> Ok s.Value
            | _ -> Error "expected a kdl int32 value, instead got: %A{v}"

    module NodeParsers =
        let node (name: string) (parser: KdlParser<'a>) : KdlParser<'a> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n -> runParser parser n.Children

        let nodeWith
            (name: string)
            (parserWith:
                KdlValue array -> Map<string, KdlValue> -> KdlParser<'a>)
            : KdlParser<'a> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n ->
                    let args = Array.ofSeq n.Arguments

                    let props =
                        n.Properties |> Seq.map (|KeyValue|) |> Map.ofSeq

                    runParser (parserWith args props) n.Children

        let children
            (parserWith:
                string
                    -> KdlValue array
                    -> Map<string, KdlValue>
                    -> KdlParser<'a>)
            : KdlParser<'a array> =
            fun doc ->
                doc.Nodes
                |> Seq.map (fun node ->
                    let args = Array.ofSeq node.Arguments

                    let props =
                        node.Properties |> Seq.map (|KeyValue|) |> Map.ofSeq

                    parserWith node.Identifier args props
                )
                |> Array.ofSeq
                |> sequenceA
                |> fun parser -> runParser parser doc

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


        let kdlParser = KdlNodesParserBuilder()

let contentDir = "recipes"

let reader = KdlReader()

open KdlParser

let loadFile (filePath: string) : Recipe =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let ingredientParser
        (name: string)
        (args: KdlValue array)
        _
        : KdlParser<Ingredient> =
        let ingredientUnitParser
            (v: KdlValue)
            : Result<IngredientUnit, string> =
            ArgParsers.str v |> Result.map OtherIngredientUnit

        let amount = ArgParsers.nth "amount" 0 ArgParsers.int args

        let ingredientUnit =
            ArgParsers.nthOpt "unit" 1 ingredientUnitParser args
            |> Result.map (Option.defaultValue Pieces)

        let variant = ArgParsers.nthOpt "variant" 2 ArgParsers.str args

        Ok Ingredient.create
        |> Result.ap (Ok name)
        |> Result.ap amount
        |> Result.ap ingredientUnit
        |> Result.ap variant
        |> ofResult

    let ingredientsParser
        (_args: KdlValue array)
        (properties: Map<string, KdlValue>)
        : KdlParser<Ingredients> =
        kdlParser {
            let! serving =
                Map.tryFind "serving" properties
                |> rtn
                |> requireSome
                    "expected to find the 'serving' property as it is required"
                |> andThen ArgParsers.int

            and! ingredients = NodeParsers.children ingredientParser

            return {
                Serving = serving
                Ingredients = Set.ofArray ingredients
            }
        }

    let recipeParser: KdlParser<Recipe> =
        kdlParser {
            let! name =
                NodeParsers.args "name" ArgParsers.str
                |> requireSingle "a recipe must only have a single name"

            and! tags = opt (NodeParsers.args "tags" ArgParsers.str)

            and! ingredients =
                NodeParsers.nodeWith "ingredients" ingredientsParser

            return {
                Name = name
                Tags = tags
                KeyInfo = None
                Ingredients = ingredients
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
