#r "../_lib/Fornax.Core.dll"
#r "nuget: FsToolkit.ErrorHandling, 4.18.0,usepackagetargets=true"
#r "nuget: Kadlet"

#load "../prelude.fsx"
#load "../parsers.fsx"

open System.IO
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
    open Parsers
    open FsToolkit.ErrorHandling

    let ingredientParser
        (_name: string)
        (args: KdlValue array)
        (_props: Map<string, KdlValue>)
        : KdlParser<Ingredient> =
        let ingredientUnitParser: KdlValueParser<IngredientUnit> =
            KdlValueParser.Primitives.str
            |> KdlValueParser.map OtherIngredientUnit

        validation {
            let! name =
                KdlValueParser.Collections.nth
                    0
                    KdlValueParser.Primitives.str
                    args

            and! amount =
                KdlValueParser.Collections.nth
                    1
                    KdlValueParser.Primitives.int32
                    args

            and! ingredientUnit =
                KdlValueParser.Collections.nthOpt 2 ingredientUnitParser args
                |> Result.map (Option.defaultValue Pieces)

            and! variant =
                KdlValueParser.Collections.nthOpt
                    3
                    KdlValueParser.Primitives.str
                    args

            return {
                Name = name
                Amount = amount
                Unit = ingredientUnit
                Variant = variant
            }
        }
        |> KdlParser.ofValidation


type Ingredients = {
    Serving: int
    Ingredients: Set<Ingredient>
}

module Ingredients =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let ingredientsParser
        (_args: KdlValue array)
        (properties: Map<string, KdlValue>)
        : KdlParser<Ingredients> =
        kdlParser {
            let! serving =
                Map.tryFind "serving" properties
                |> Result.requireSome [
                    "expected to find the required 'serving' property, but found none"
                ]
                |> Result.bind (
                    KdlValueParser.runParser KdlValueParser.Primitives.int32
                )
                |> KdlParser.ofValidation

            and! ingredients =
                KdlParser.Combinators.children Ingredient.ingredientParser

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
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    // TODO: Verify that the nodes here are named "i"?
    let private keyInfoParser: KdlParser<Map<string, string>> =
        let keyInfoChildrenParser _ args _ =
            validation {
                let! name =
                    KdlValueParser.Collections.nth
                        0
                        KdlValueParser.Primitives.str
                        args

                and! value =
                    KdlValueParser.Collections.nth
                        1
                        KdlValueParser.Primitives.str
                        args

                return name, value
            }
            |> KdlParser.ofValidation


        KdlParser.Combinators.children keyInfoChildrenParser
        |> KdlParser.map Map.ofSeq


    let private instructionsParser
        (args: KdlValue array)
        _props
        : KdlParser<string array> =
        args
        |> Seq.map (
            KdlValueParser.runParser KdlValueParser.Primitives.str
            >> Result.mapError (String.concat "; ")
        )
        |> Seq.sequenceResultA
        |> Result.mapError List.ofSeq
        |> Result.map Array.ofSeq
        |> KdlParser.ofValidation

    let parser: KdlParser<Recipe> =
        let firstArg
            (parser: KdlValueParser<'a>)
            (args: KdlValue array)
            _
            : KdlDocument -> Validation<'a, string> =
            KdlValueParser.Collections.nth 0 parser args
            |> KdlParser.ofValidation

        let allArgs
            (parser: KdlValueParser<'a>)
            (args: KdlValue array)
            _
            : KdlDocument -> Validation<'a array, string> =
            args
            |> Array.map (
                KdlValueParser.runParser parser
                >> Result.mapError (String.concat "; ")
            )
            |> Seq.sequenceResultA
            |> Result.mapError List.ofSeq
            |> Result.map Array.ofSeq
            |> KdlParser.ofValidation

        kdlParser {
            let! name =
                KdlParser.Combinators.nodeWith
                    "name"
                    (firstArg KdlValueParser.Primitives.str)

            and! tags =
                KdlParser.opt (
                    KdlParser.Combinators.nodeWith
                        "tags"
                        (allArgs KdlValueParser.Primitives.str)
                )

            and! keyInfo =
                KdlParser.opt (
                    KdlParser.Combinators.node "key-info" keyInfoParser
                )

            and! ingredients =
                KdlParser.Combinators.nodeWith
                    "ingredients"
                    Ingredients.ingredientsParser

            and! instructions =
                KdlParser.Combinators.nodeWith "instructions" instructionsParser

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

    let p = Parsers.KdlParser.Combinators.node "recipe" Recipe.parser

    match Parsers.KdlParser.runParser p doc with
    | Ok r -> r
    | Error reasons ->
        let reason = String.concat "; " reasons
        failwith $"could not parse: %s{reason}"

let loader (projectRoot: string) (siteContent: SiteContents) =
    let recipesPath = Path.Combine(projectRoot, contentDir)
    let options = EnumerationOptions(RecurseSubdirectories = true)
    let files = Directory.GetFiles(recipesPath, "*", options)

    files
    |> Array.filter (fun s -> s.EndsWith ".kdl")
    |> Array.map loadFile
    |> Array.iter siteContent.Add

    siteContent
