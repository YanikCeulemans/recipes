#r "../_lib/Fornax.Core.dll"
#r "nuget: FsToolkit.ErrorHandling, 4.18.0,usepackagetargets=true"
#r "nuget: Kadlet"

#load "../prelude.fsx"
#load "../parsers.fsx"

open System
open System.IO
open Kadlet
open Prelude

type Duration = MkDuration of TimeSpan

module Duration =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let extract (MkDuration duration) = duration

    let parseDurationAmount
        (key: string)
        (value: KdlValue)
        : Validation<TimeSpan, string> =
        let parsedValue =
            KdlValueParser.runParser KdlValueParser.Primitives.int32 value

        match key, parsedValue with
        | "minutes", Ok n -> Validation.ok (int64 n |> TimeSpan.FromMinutes)
        | other, _ ->
            Validation.error $"unsupported duration amount type '{other}'"


    let parser _args (properties: Map<string, KdlValue>) : KdlParser<Duration> =
        kdlParser {
            let! duration =
                match List.ofSeq properties with
                | [] ->
                    KdlParser.fail
                        "the duration node requires at least one property"
                | KeyValue(key, value) :: kvps ->
                    validation {
                        let! durationAmount = parseDurationAmount key value

                        let! durationAmounts =
                            kvps
                            |> Seq.map (|KeyValue|)
                            |> Validation.traverse (fun (key, value) ->
                                parseDurationAmount key value
                            )

                        return
                            Seq.fold
                                (fun (acc: TimeSpan) (curr: TimeSpan) ->
                                    acc.Add curr
                                )
                                durationAmount
                                durationAmounts
                    }
                    |> KdlParser.ofValidation

            return MkDuration duration
        }

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

    let append
        (amountA: IngredientAmount)
        (amountB: IngredientAmount)
        : IngredientAmount option =
        match amountA, amountB with
        | ToTaste, ToTaste -> Some ToTaste

        | Pieces nA, Pieces nB -> Some(Pieces(nA + nB))

        | OtherIngredientUnit(nA, uA), OtherIngredientUnit(nB, uB) when uA = uB ->
            Some(OtherIngredientUnit(nA + nB, uA))

        | otherA, otherB -> None

type Ingredient = {
    Name: string
    Amount: IngredientAmount
    Variant: string option
}

module Ingredient =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let isToTaste (ingredient: Ingredient) =
        match ingredient.Amount with
        | ToTaste -> true
        | _ -> false

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

type RecipeImage =
    | ExternalImage of Uri
    | InternalImage of string

module RecipeImage =
    open FsToolkit.ErrorHandling

    let ofString (candidate: string) =
        match candidate with
        | FileUri u -> Validation.ok (InternalImage u.AbsolutePath)
        | ExternalUri u -> Validation.ok (ExternalImage u)
        | _ ->
            Validation.error
                $"could not parse candidate {candidate} into a RecipeImage"

    let isExternalImage =
        function
        | ExternalImage _ -> true
        | InternalImage _ -> false

    let format =
        function
        | ExternalImage uri -> uri.AbsoluteUri
        | InternalImage p -> p

type Step = {
    Description: string
    Ingredients: Ingredient Set
}

module Step =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let parser (node: KdlNode) : KdlParser<Step> =
        kdlParser {
            let! description =
                node.Arguments
                |> Array.ofSeq
                |> KdlValueParser.Collections.nth
                    0
                    KdlValueParser.Primitives.str
                |> KdlParser.ofValidation

            and! ingredients =
                KdlParser.Combinators.childrenNamed
                    "ingredient"
                    Ingredient.ingredientParser
                |> KdlParser.map Set.ofArray

            return {
                Description = description
                Ingredients = ingredients
            }
        }

type Instructions = { Serving: int; Steps: Step array }

module Instructions =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

    let ingredients (instructions: Instructions) =
        let rec go (acc: Ingredient list) (ingredient: Ingredient) =
            let existing =
                List.indexed acc
                |> List.tryFind (fun (i, innerIngredient) ->
                    innerIngredient.Name = ingredient.Name
                )

            match existing with
            | None -> ingredient :: acc
            | Some(i, fi) ->
                match IngredientAmount.append fi.Amount ingredient.Amount with
                | Some updatedAmount ->
                    let updatedIngredient = { fi with Amount = updatedAmount }

                    List.updateAt i updatedIngredient acc
                | None -> ingredient :: acc

        [
            for step in instructions.Steps do
                yield! step.Ingredients
        ]
        |> List.fold go []
        |> List.rev

    let parser
        _args
        (properties: Map<string, KdlValue>)
        : KdlParser<Instructions> =
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

            and! steps = KdlParser.Combinators.childrenNamed "step" Step.parser

            return { Serving = serving; Steps = steps }
        }

type Recipe = {
    Name: string
    Tags: string Set option
    Duration: Duration
    Image: RecipeImage
    Instructions: Instructions
}

module Recipe =
    open Parsers
    open Parsers.KdlParser.ComputationExpression
    open FsToolkit.ErrorHandling

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
                |> KdlParser.map (Option.map Set.ofArray)

            and! duration =
                KdlParser.Combinators.nodeWith "duration" Duration.parser

            and! image =
                KdlParser.Combinators.nodeWith
                    "image"
                    (firstArg KdlValueParser.Primitives.str)
                |> KdlParser.andThen RecipeImage.ofString

            and! instructions =
                KdlParser.Combinators.nodeWith
                    "instructions"
                    Instructions.parser

            return {
                Name = name
                Tags = tags
                Duration = duration
                Image = image
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
        Path.modifyExt (always ".html") filePath |> String.replace rootDir "/"

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
