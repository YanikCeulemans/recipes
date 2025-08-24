#r "nuget: Imageflow.AllPlatforms"
#r "../_lib/Fornax.Core.dll"
#load "../prelude.fsx"

open System
open System.IO
open System.Text.Json
open Imageflow.Fluent
open Prelude

let hashCacheFileName = ".hash-cache"

let computeHash (bytes: byte array) =
    System.Security.Cryptography.SHA1.HashData bytes
    |> System.Convert.ToBase64String

let readHashCache (page: string) =
    let dir = Path.GetDirectoryName page

    try
        use strm = File.OpenRead(Path.Combine(dir, hashCacheFileName))
        JsonSerializer.Deserialize<Map<string, string>> strm |> Some
    with e ->
        eprintfn $"could not read .hash-cache: {e}"
        None

let writeHashCache (page: string) (hashCache: Map<string, string>) =
    let dir = Path.GetDirectoryName page
    let hashCachePath = Path.Combine(dir, hashCacheFileName)
    File.WriteAllText(hashCachePath, JsonSerializer.Serialize hashCache)

let withHashCache
    (page: string)
    (f: Map<string, string> -> Map<string, string> * 'a)
    : 'a =
    let hc = readHashCache page |> Option.defaultValue Map.empty
    let newHc, result = f hc

    if hc <> newHc then
        writeHashCache page newHc

    result

module Performance =
    open System.Diagnostics

    let time (label: string) (f: Unit -> 'a) : 'a =
        let mark = Stopwatch.GetTimestamp()
        let r = f ()

        printfn
            $"{label} took: {Stopwatch.GetElapsedTime mark |> fun x -> x.ToString()}"

        r

let generateImages
    (projectRoot: string)
    (page: string)
    (hashCache: Map<string, string>)
    =
    let inputPath = Path.Combine(projectRoot, page)

    let imagesBytes =
        Performance.time "fileOpen" (fun () -> File.ReadAllBytes inputPath)

    let inputHash =
        Performance.time "compute hash" (fun () -> computeHash imagesBytes)

    match Map.tryFind page hashCache with
    | Some hash when hash = inputHash ->
        printfn $"image {page} already processed, skipping"
        hashCache, []
    | Some _
    | None ->
        use imageJob = new ImageJob()

        let r =
            imageJob
                .Decode(imagesBytes)
                .Constrain(Constraint(ConstraintMode.Aspect_Crop, 16u, 9u))
                .Branch(fun n ->
                    n
                        .ResizerCommands("width=400")
                        .EncodeToBytes(WebPLossyEncoder 80f)
                )
                .EncodeToBytes(WebPLossyEncoder 80f)
                .Finish()
                .InProcessAndDisposeAsync()
                .Result

        let thumbnailBytes = (r.TryGet 1).TryGetBytes().Value |> Array.ofSeq
        let croppedBytes = (r.TryGet 2).TryGetBytes().Value |> Array.ofSeq

        let imagePathsAndBytes = [
            Path.ChangeExtension(page, ".webp")
            |> Path.modifyFileName (String.prefix "thumbnail-"),
            thumbnailBytes
            Path.ChangeExtension(page, ".webp"), croppedBytes
        ]

        Map.add page inputHash hashCache, imagePathsAndBytes

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    let pageFullPath = Path.Combine(projectRoot, page)

    Performance.time
        "generate image"
        (fun () -> withHashCache page (generateImages projectRoot page))
