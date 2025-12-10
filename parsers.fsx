#r "nuget: Kadlet"
#r "nuget: FsToolkit.ErrorHandling, 4.18.0,usepackagetargets=true"

#load "prelude.fsx"

open FsToolkit.ErrorHandling
open Kadlet

module Node =
    let args (node: KdlNode) : KdlValue array = node.Arguments |> Array.ofSeq

    let props (node: KdlNode) : Map<string, KdlValue> =
        node.Properties |> Seq.map (|KeyValue|) |> Map.ofSeq

type KdlParser<'a> = KdlDocument -> Validation<'a, string>

module KdlParser =
    let runParser
        (parser: KdlParser<'a>)
        (doc: KdlDocument)
        : Validation<'a, string> =
        parser doc

    let rtn (x: 'a) : KdlParser<'a> = fun _ -> Ok x

    let fail (err: string) : KdlParser<'a> = fun _ -> Validation.error err

    let map (f: 'a -> 'b) (ax: KdlParser<'a>) : KdlParser<'b> =
        fun doc ->
            let a = runParser ax doc
            Validation.map f a

    let andThen
        (f: 'a -> Validation<'b, string>)
        (ma: KdlParser<'a>)
        : KdlParser<'b> =
        fun doc ->
            let a = runParser ma doc
            Validation.bind f a

    let alt (parserB: KdlParser<'a>) (parserA: KdlParser<'a>) : KdlParser<'a> =
        fun doc ->
            match runParser parserA doc with
            | Ok a -> Ok a
            | Error errsA ->
                match runParser parserB doc with
                | Ok b -> Ok b
                | Error errsB -> Error(errsA @ errsB)

    let opt (parser: KdlParser<'a>) : KdlParser<'a option> =
        parser |> map Some |> alt (rtn None)

    let ofValidation (result: Validation<'a, string>) : KdlParser<'a> =
        fun _doc -> result

    let ap
        (parserA: KdlParser<'a>)
        (parserF: KdlParser<'a -> 'b>)
        : KdlParser<'b> =
        fun doc ->
            let resA = runParser parserA doc
            let resF = runParser parserF doc

            Validation.apply resF resA

    let sequenceA (parsers: KdlParser<'a> array) : KdlParser<'a array> =
        let go curr acc =
            rtn Prelude.List.cons |> ap curr |> ap acc

        Array.foldBack go parsers (rtn []) |> map Array.ofList

    module Combinators =
        let private docNodes (doc: KdlDocument) =
            option {
                let! doc = Option.ofObj doc
                let! nodes = Option.ofObj doc.Nodes
                return seq nodes
            }

        let private firstNodeNamed (name: string) (nodes: KdlNode seq) =
            nodes |> Seq.tryFind (fun n -> n.Identifier = name)

        let node (name: string) (parser: KdlParser<'a>) : KdlParser<'a> =
            fun doc ->
                match docNodes doc |> Option.bind (firstNodeNamed name) with
                | None ->
                    Validation.error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n -> runParser parser n.Children

        let singleNodeWith (parser: KdlNode -> KdlParser<'a>) : KdlParser<'a> =
            fun doc ->
                match
                    docNodes doc |> Option.defaultValue Seq.empty |> List.ofSeq
                with
                | [ n ] -> parser n doc
                | other ->
                    Validation.error
                        $"expected only a single node, instead got: %A{other}"

        let nodeWith
            (name: string)
            (parserWith:
                KdlValue array -> Map<string, KdlValue> -> KdlParser<'a>)
            : KdlParser<'a> =
            fun doc ->
                match firstNodeNamed name doc.Nodes with
                | None ->
                    Validation.error
                        $"expected to find node named '{name}' in kdl document:\n{doc.ToKdlString()}"
                | Some n ->
                    let args = Array.ofSeq n.Arguments

                    let props =
                        n.Properties |> Seq.map (|KeyValue|) |> Map.ofSeq

                    runParser (parserWith args props) n.Children

        let childrenWith
            (parserWith:
                string
                    -> KdlValue array
                    -> Map<string, KdlValue>
                    -> KdlParser<'a>)
            : KdlParser<'a array> =
            fun doc ->
                docNodes doc
                |> Option.defaultValue Seq.empty
                |> Seq.map (fun node ->
                    let args = Array.ofSeq node.Arguments

                    let props =
                        node.Properties |> Seq.map (|KeyValue|) |> Map.ofSeq

                    parserWith node.Identifier args props
                )
                |> Array.ofSeq
                |> sequenceA
                |> fun parser -> runParser parser doc

        let childrenNamed
            (name: string)
            (parser: KdlNode -> KdlParser<'a>)
            : KdlParser<'a array> =
            fun doc ->
                docNodes doc
                |> Option.defaultValue Seq.empty
                |> Seq.filter (fun node -> node.Identifier = name)
                |> Seq.map (fun node -> runParser (parser node) node.Children)
                |> Array.ofSeq
                |> Prelude.Validation.traverse id
                |> Validation.map Array.ofSeq
                |> fun vs -> ofValidation vs doc

    module ComputationExpression =
        type KdlNodesParserBuilder() =
            member _.BindReturn(ax, f) = map f ax

            member _.MergeSources
                (ma: KdlParser<'a>, mb: KdlParser<'b>)
                : KdlParser<'a * 'b> =
                rtn Prelude.Tuple.create |> ap ma |> ap mb


        let kdlParser = KdlNodesParserBuilder()

type KdlValueParser<'a> = KdlValue -> Validation<'a, string>

module KdlValueParser =
    let runParser
        (parser: KdlValueParser<'a>)
        (value: KdlValue)
        : Result<'a, string list> =

        parser value

    let map (f: 'a -> 'b) (parser: KdlValueParser<'a>) : KdlValueParser<'b> =
        fun value -> runParser parser value |> Validation.map f

    let apply
        (ma: KdlValueParser<'a>)
        (mf: KdlValueParser<'a -> 'b>)
        : KdlValueParser<'b> =
        fun value ->
            let a = runParser ma value
            let f = runParser mf value

            Validation.apply f a

    let alt
        (parserB: KdlValueParser<'a>)
        (parserA: KdlValueParser<'a>)
        : KdlValueParser<'a> =
        fun value ->
            match runParser parserA value with
            | Ok x -> Ok x
            | Error errsA ->
                match runParser parserB value with
                | Ok x -> Ok x
                | Error errsB -> Error(errsA @ errsB)

    module Primitives =
        let str: KdlValueParser<string> =
            function
            | :? KdlString as p -> Validation.ok p.Value
            | other ->
                Validation.error
                    $"expected KDL string value, instead got: '%A{other}'"

        let int32: KdlValueParser<int> =
            function
            | :? KdlInt32 as p -> Validation.ok p.Value
            | other ->
                Validation.error
                    $"expected KDL int32 value, instead got: '%A{other}'"

        let float: KdlValueParser<float> =
            function
            | :? KdlFloat64 as p -> Validation.ok p.Value
            | other ->
                Validation.error
                    $"expected KDL float value, instead got: '%A{other}'"

    module Collections =
        let nth
            (index: int)
            (parser: KdlValueParser<'a>)
            (args: KdlValue array)
            : Validation<'a, string> =
            Array.tryItem index args
            |> Result.requireSome [
                $"expected to find argument at index {index}, but none was found"
            ]
            |> Result.bind (runParser parser)

        let nthOpt
            (index: int)
            (parser: KdlValueParser<'a>)
            (args: KdlValue array)
            : Validation<'a option, string> =
            match Array.tryItem index args with
            | None -> Validation.ok None
            | Some a -> runParser parser a |> Validation.map Some
