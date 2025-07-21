#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"

open Html


let generate' (ctx: SiteContents) (page: string) =
    let recipe =
        ctx.TryGetValues<Recipeloader.Recipe>()
        |> Option.defaultValue Seq.empty
        // |> Seq.find (fun n -> n.File = page)
        |> Seq.find (fun n -> true)

    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>()

    let desc =
        siteInfo
        |> Option.map (fun si -> si.description)
        |> Option.defaultValue ""

    Layout.layout ctx recipe.Name [
        section [ Class "hero is-info is-medium is-bold" ] [
            div [ Class "hero-body" ] [
                div [ Class "container has-text-centered" ] [
                    h1 [ Class "title" ] [ !!desc ]
                ]
            ]
        ]
        div [ Class "container" ] [
            section [ Class "articles" ] [
                div [ Class "column is-8 is-offset-2" ] [
                    Layout.recipeLayout recipe
                ]
            ]
        ]
    ]

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    generate' ctx page |> Layout.render ctx
