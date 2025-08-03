#r "nuget: Imageflow.AllPlatforms"
#r "../_lib/Fornax.Core.dll"

open System
open System.IO
open Imageflow.Fluent

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    printfn "recipe images %s" page

    let inputPath = Path.Combine(projectRoot, page)
    let imagesBytes = File.ReadAllBytes inputPath
    use imageJob = new ImageJob()

    let r =
        imageJob
            .Decode(imagesBytes)
            .Constrain(Constraint(ConstraintMode.Aspect_Crop, 4u, 3u))
            .EncodeToBytes(LodePngEncoder())
            .Finish()
            .InProcessAndDisposeAsync()
            .Result

    r.First.TryGetBytes().Value |> Array.ofSeq
