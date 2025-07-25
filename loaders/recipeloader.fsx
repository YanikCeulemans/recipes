#r "../_lib/Fornax.Core.dll"
#r "nuget: Kadlet"

#load "../prelude.fsx"
#load "../parsers.fsx"

open System.IO
open Prelude
open Kadlet

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

    open Parsers
    open Parsers.KdlParser

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

        Ok create
        |> Result.ap (Ok name)
        |> Result.ap amount
        |> Result.ap ingredientUnit
        |> Result.ap variant
        |> ofResult


type Ingredients = {
    Serving: int
    Ingredients: Set<Ingredient>
}

module Ingredients =
    open Parsers
    open Parsers.KdlParser

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

            and! ingredients = NodeParsers.children Ingredient.ingredientParser

            return {
                Serving = serving
                Ingredients = Set.ofArray ingredients
            }
        }


type Recipe = {
    Name: string
    Tags: string array option
    KeyInfo: Map<string, string> option
    Ingredients: Ingredients
    Instructions: string array
}

module Recipe =
    open Parsers
    open Parsers.KdlParser

    let private keyInfoParser: KdlParser<Map<string, string>> =
        let keyInfoChildrenParser name args _ =
            ArgParsers.nth "Key info value" 0 ArgParsers.str args
            |> Result.map (fun v -> name, v)
            |> ofResult

        NodeParsers.children keyInfoChildrenParser |> KdlParser.map Map.ofSeq


    let private instructionsParser
        (args: KdlValue array)
        _props
        : KdlParser<string array> =
        args
        |> Seq.map ArgParsers.str
        |> Result.Traversable.sequenceA
        |> Result.map Array.ofSeq
        |> ofResult

    let parser: KdlParser<Recipe> =
        kdlParser {
            let! name =
                NodeParsers.args "name" ArgParsers.str
                |> requireSingle "a recipe must only have a single name"

            and! tags = opt (NodeParsers.args "tags" ArgParsers.str)

            and! keyInfo = opt (NodeParsers.node "key-info" keyInfoParser)

            and! ingredients =
                NodeParsers.nodeWith "ingredients" Ingredients.ingredientsParser

            and! instructions =
                NodeParsers.nodeWith "instructions" instructionsParser

            return {
                Name = name
                Tags = tags
                KeyInfo = keyInfo
                Ingredients = ingredients
                Instructions = instructions
            }
        }

let contentDir = "recipes"

let reader = KdlReader()

let loadFile (filePath: string) : Recipe =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let p = Parsers.KdlParser.NodeParsers.node "recipe" Recipe.parser

    match Parsers.KdlParser.runParser p doc with
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
