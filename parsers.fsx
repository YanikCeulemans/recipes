#r "nuget: Kadlet"

#load "prelude.fsx"

open Prelude
open Kadlet

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
                    |> Result.Traversable.sequenceA
                    |> Result.map Array.ofSeq

    [<AutoOpen>]
    module ComputationExpression =
        type KdlNodesParserBuilder() =
            member _.BindReturn(ax, f) = map f ax

            member _.MergeSources
                (ma: KdlParser<'a>, mb: KdlParser<'b>)
                : KdlParser<'a * 'b> =
                rtn Tuple.create |> ap ma |> ap mb


        let kdlParser = KdlNodesParserBuilder()
