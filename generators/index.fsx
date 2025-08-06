#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"

open Html

let generate' (ctx: SiteContents) (_: string) =
    let recipes =
        ctx.TryGetValues<Recipeloader.RecipeEnvelope>()
        |> Option.defaultValue Seq.empty

    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>()

    let _, postPageSize =
        siteInfo
        |> Option.map (fun si -> si.description, si.postPageSize)
        |> Option.defaultValue ("", 10)


    let rcps =
        recipes
        |> Seq.sortByDescending (fun r -> r.Recipe.Name)
        |> Seq.toList
        |> List.chunkBySize postPageSize
        |> List.map (List.map Layout.recipeSummary)

    let pages = List.length rcps

    let getFilenameForIndex i =
        if i = 0 then
            sprintf "index.html"
        else
            sprintf "posts/page%i.html" i

    let layoutForPostSet i rcps =
        let nextPage =
            if i = pages - 1 then
                "#"
            else
                "/" + getFilenameForIndex (i + 1)

        let previousPage =
            if i = 0 then "#" else "/" + getFilenameForIndex (i - 1)

        Layout.layout ctx "Home" [
            div [ Class "container" ] [
                section [ Class "my-6" ] [
                    div [ Class "fixed-grid has-3-cols" ] [
                        div [ Class "grid is-gap-3" ] [
                            for recipe in rcps do
                                div [ Class "cell" ] [ recipe ]
                        ]
                    ]
                ]
            ]
            div [ Class "container" ] [
                div [ Class "has-text-centered my-6" ] [
                    a [ Href previousPage ] [ !!"Previous" ]
                    !!(sprintf "%i of %i" (i + 1) pages)
                    a [ Href nextPage ] [ !!"Next" ]
                ]
            ]
        ]

    rcps
    |> List.mapi (fun i rcps ->
        getFilenameForIndex i, layoutForPostSet i rcps |> Layout.render ctx
    )

let generate (ctx: SiteContents) (projectRoot: string) (page: string) =
    generate' ctx page
