#r "../_lib/Fornax.Core.dll"
#r "nuget: Kadlet"

open System.IO
open Kadlet

module Result =
    let sequenceA (xs: Result<'a, string> seq) : Result<'a seq, string> =
        let go curr acc =
            match acc, curr with
            | Error errAcc, Error errCurr -> Error $"{errCurr}; {errAcc}"
            | Error err, _
            | _, Error err -> Error err
            | Ok a, Ok c -> Ok(c :: a)

        Seq.foldBack go xs (Ok []) |> Result.map seq

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

type KdlNodesParser<'a> = KdlDocument -> Result<'a, string>

module KdlNodesParser =
    let runParser (parser: KdlNodesParser<'a>) doc : Result<'a, string> =
        parser doc

    let rtn (x: 'a) : KdlNodesParser<'a> = fun _ -> Ok x

    let map (f: 'a -> 'b) (ax: KdlNodesParser<'a>) : KdlNodesParser<'b> =
        fun doc ->
            let a = runParser ax doc
            Result.map f a

    let private firstNodeNamed (name: string) (nodes: KdlNode seq) =
        nodes |> Seq.tryFind (fun n -> n.Identifier = name)

    module ArgParsers =
        let str (v: KdlValue) : Result<string, string> =
            match v with
            | :? KdlString as s -> Ok s.Value
            | _ -> Error "expected a kdl string value, instead got: %A{v}"

    module NodeParsers =
        let node
            (name: string)
            (parser: KdlNodesParser<'a>)
            : KdlNodesParser<'a> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n -> runParser parser n.Children

        let nodeArgs
            (name: string)
            (argParser: KdlValue -> Result<'a, string>)
            : KdlNodesParser<'a array> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n ->
                    n.Arguments
                    |> Seq.map argParser
                    |> Result.sequenceA
                    |> Result.map Array.ofSeq

    // let str: KdlNodesParser<string> = failwith "todo"

    // let many (parser: KdlNodesParser<'a>) : KdlNodesParser<'a array> =
    //     failwith "todo"

    [<AutoOpen>]
    module ComputationExpression =
        type KdlNodesParserBuilder() =
            member _.BindReturn(ax, f) = map f ax

            member _.MergeSources
                (ma: KdlNodesParser<'a>, mb: KdlNodesParser<'b>)
                : KdlNodesParser<'a * 'b> =
                fun nodes ->
                    let resA = runParser ma nodes
                    let resB = runParser mb nodes

                    match resA, resB with
                    | Error errA, Error errB -> Error($"{errA}; {errB}")
                    | Error err, _
                    | _, Error err -> Error err
                    | Ok a, Ok b -> Ok(a, b)


        let kdlNodesParser = KdlNodesParserBuilder()

let contentDir = "recipes"

let reader = KdlReader()

open KdlNodesParser

let loadFile (filePath: string) : Recipe =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let recipeParser: KdlNodesParser<Recipe> =
        kdlNodesParser {
            let! name = NodeParsers.nodeArgs "name" ArgParsers.str
            and! tags = NodeParsers.nodeArgs "tags" ArgParsers.str

            return {
                Name = name |> Array.head // TODO: rework to expect a single arg
                Tags = Some tags // TODO: implement optional parsing
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
