#r "../_lib/Fornax.Core.dll"
#r "nuget: FsToolkit.ErrorHandling, 4.18.0,usepackagetargets=true"
#r "nuget: Kadlet"

#load "../prelude.fsx"
#load "../parsers.fsx"

open System
open System.IO
open Kadlet
open Prelude

type IngredientAmount =
    | ToTaste
    | Pieces of int
    | OtherIngredientUnit of int * string

module IngredientAmount =
    let format =
        function
        | ToTaste -> "to taste"
        | Pieces n -> $"%d{n}"
        | OtherIngredientUnit(n, u) -> $"%d{n} %s{u}"

    let formatWithScaling (scaling: string) amount =
        match amount with
        | ToTaste -> None, "to taste"
        | Pieces n -> Some($"(%s{scaling}) * %d{n}", n), ""
        | OtherIngredientUnit(n, u) -> Some($"(%s{scaling}) * %d{n}", n), u

type Ingredient = {
    Name: string
    Amount: IngredientAmount
    Variant: string option
}

module Ingredient =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let private amountParser (node: KdlNode) : KdlParser<IngredientAmount> =
        match node.Identifier with
        | "amount" ->
            let args = Node.args node

            validation {
                let! amount =
                    KdlValueParser.Collections.nth
                        0
                        KdlValueParser.Primitives.int32
                        args

                and! ingredientUnit =
                    KdlValueParser.Collections.nthOpt
                        1
                        KdlValueParser.Primitives.str
                        args

                match ingredientUnit with
                | None -> return Pieces amount
                | Some ingredientUnit ->
                    return OtherIngredientUnit(amount, ingredientUnit)
            }
            |> KdlParser.ofValidation
        | "to-taste" -> KdlParser.rtn ToTaste
        | _ -> KdlParser.fail $"unexpected amount named: %s{node.Identifier}"

    let ingredientParser (node: KdlNode) : KdlParser<Ingredient> =
        let args = Node.args node

        kdlParser {
            let! ingredientAmount =
                KdlParser.runParser
                    (KdlParser.Combinators.singleNodeWith amountParser)
                    node.Children
                |> KdlParser.ofValidation

            and! name =
                KdlValueParser.Collections.nth
                    0
                    KdlValueParser.Primitives.str
                    args
                |> KdlParser.ofValidation

            and! variant =
                KdlValueParser.Collections.nthOpt
                    1
                    KdlValueParser.Primitives.str
                    args
                |> KdlParser.ofValidation

            return {
                Name = name
                Amount = ingredientAmount
                Variant = variant
            }
        }

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
                KdlParser.Combinators.childrenNamed
                    "ingredient"
                    Ingredient.ingredientParser

            return {
                Serving = serving
                Ingredients = Set.ofArray ingredients
            }
        }

type RecipeImage =
    | ExternalImage of Uri
    | InternalImage of string

module RecipeImage =
    open FsToolkit.ErrorHandling

    let ofString (candidate: string) =
        match candidate with
        | Uri u -> Validation.ok (ExternalImage u)
        | _ -> Validation.ok (InternalImage candidate)

    let isExternalImage =
        function
        | ExternalImage _ -> true
        | InternalImage _ -> false

    let format =
        function
        | ExternalImage uri -> uri.AbsoluteUri
        | InternalImage p -> p

type Recipe = {
    Name: string
    Tags: string array option
    KeyInfo: Map<string, string> option
    Image: RecipeImage
    Ingredients: Ingredients
    Instructions: string array
}

module Recipe =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let private keyInfoParser: KdlParser<Map<string, string>> =
        let keyInfoChildrenParser (node: KdlNode) =
            let args = Node.args node

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


        KdlParser.Combinators.childrenNamed "info" keyInfoChildrenParser
        |> KdlParser.map Map.ofSeq


    let private instructionStepParser (node: KdlNode) =
        let args = Node.args node

        KdlValueParser.Collections.nth 0 KdlValueParser.Primitives.str args
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

            and! image =
                KdlParser.Combinators.nodeWith
                    "image"
                    (firstArg KdlValueParser.Primitives.str)
                |> KdlParser.andThen RecipeImage.ofString

            and! ingredients =
                KdlParser.Combinators.nodeWith
                    "ingredients"
                    Ingredients.ingredientsParser

            and! instructions =
                KdlParser.Combinators.node
                    "instructions"
                    (KdlParser.Combinators.childrenNamed
                        "step"
                        instructionStepParser)

            return {
                Name = name
                Tags = tags
                KeyInfo = keyInfo
                Image = image
                Ingredients = ingredients
                Instructions = instructions
            }
        }

type RecipeEnvelope = {
    FileName: string
    Link: string
    Recipe: Recipe
}

let contentDir = "recipes"

let reader = KdlReader()

let loadFile (rootDir: string) (filePath: string) : RecipeEnvelope =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let p = Parsers.KdlParser.Combinators.node "recipe" Recipe.parser

    let link =
        Path.modifyExt (always ".html") filePath |> String.replace rootDir ""

    match Parsers.KdlParser.runParser p doc with
    | Ok r -> {
        FileName = filePath
        Link = link
        Recipe = r
      }
    | Error reasons ->
        let reason = String.concat "; " reasons
        failwith $"could not parse kdl doc for file %s{filePath}: %s{reason}"

let loader (projectRoot: string) (siteContent: SiteContents) =
    let recipesPath = Path.Combine(projectRoot, contentDir)
    let options = EnumerationOptions(RecurseSubdirectories = true)
    let files = Directory.GetFiles(recipesPath, "*", options)

    files
    |> Array.filter (fun s -> s.EndsWith ".kdl")
    |> Array.map (loadFile projectRoot)
    |> Array.iter siteContent.Add

    siteContent
