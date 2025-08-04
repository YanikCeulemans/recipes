#r "nuget: Imageflow.AllPlatforms"
#r "../_lib/Fornax.Core.dll"
#load "../prelude.fsx"

open System.IO
open Imageflow.Fluent
open Prelude

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    let inputPath = Path.Combine(projectRoot, page)
    let imagesBytes = File.ReadAllBytes inputPath
    use imageJob = new ImageJob()

    let r =
        imageJob
            .Decode(imagesBytes)
            .Constrain(Constraint(ConstraintMode.Aspect_Crop, 4u, 3u))
            .Branch(fun n ->
                n
                    .ResizerCommands("width=400&height=300")
                    .EncodeToBytes(WebPLossyEncoder 80f)
            )
            .EncodeToBytes(WebPLossyEncoder 80f)
            .Finish()
            .InProcessAndDisposeAsync()
            .Result

    let thumbnailBytes = (r.TryGet 1).TryGetBytes().Value |> Array.ofSeq
    let croppedBytes = (r.TryGet 2).TryGetBytes().Value |> Array.ofSeq

    [
        Path.ChangeExtension(page, ".webp")
        |> Path.modifyFileName (String.prefix "thumbnail-"),
        thumbnailBytes
        Path.ChangeExtension(page, ".webp"), croppedBytes
    ]
