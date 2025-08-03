#r "../_lib/Fornax.Core.dll"

open System.IO

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    printfn "recipe images %s" page

    let inputPath = Path.Combine(projectRoot, page)
    File.ReadAllBytes inputPath
