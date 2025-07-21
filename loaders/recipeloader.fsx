#r "../_lib/Fornax.Core.dll"
#r "nuget: Kadlet"

open System
open System.IO
open Kadlet


let contentDir = "recipes"

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


    module NodeParsers =
        let node
            (name: string)
            (parser: KdlNodesParser<'a>)
            : KdlNodesParser<'a> =
            fun doc ->
                match doc.Nodes |> Seq.tryFind (_.Identifier >> (=) name) with
                | None ->
                    Error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n -> runParser parser n.Children

        let nodeArgs args = failwith ""

        let str: KdlNodesParser<string> = failwith "todo"

        let many (parser: KdlNodesParser<'a>) : KdlNodesParser<'a array> =
            failwith "todo"

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

let reader = KdlReader()

open KdlNodesParser

let loadFile (filePath: string) : Recipe =
    use fs = File.OpenRead filePath
    let doc = reader.Parse fs

    let p: KdlNodesParser<Recipe> =
        kdlNodesParser {
            let! name = NodeParsers.node "name" NodeParsers.str

            and! tags =
                NodeParsers.node "tags" (NodeParsers.many NodeParsers.str)

            return {
                Name = ""
                Tags = None
                KeyInfo = None
                Ingredients = { Serving = 0; Ingredients = Set.empty }
                Instructions = [||]
            }
        }

    match runParser p doc with
    | Ok r -> r
    | Error reason -> failwith $"could not parse: %s{reason}"


let loader (projectRoot: string) (siteContent: SiteContents) =
    let recipesPath = Path.Combine(projectRoot, contentDir)
    let options = EnumerationOptions(RecurseSubdirectories = true)
    let files = Directory.GetFiles(recipesPath, "*", options)

    files
    |> Array.filter _.EndsWith(".kdl")
    |> Array.map loadFile
    |> Array.iter siteContent.Add

    siteContent
