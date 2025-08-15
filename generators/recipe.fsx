#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"

open Html


let generate' (ctx: SiteContents) (page: string) =
    let recipeEnvelope =
        ctx.TryGetValues<Recipeloader.RecipeEnvelope>()
        |> Option.defaultValue Seq.empty
        |> Seq.find (fun n -> n.FileName.EndsWith page)

    Layout.layout ctx (Some recipeEnvelope.Recipe.Name) [
        div [ Class "container" ] [
            section [] [ Layout.recipeLayout recipeEnvelope ]
        ]
    ]

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    generate' ctx page |> Layout.render ctx
