#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"
#load "../prelude.fsx"

open Html

let classes cs = String.concat " " cs |> Class

let generate' (ctx: SiteContents) =
    let layoutForRecipes (rcps: Recipeloader.RecipeEnvelope seq) =
        Layout.layout
            ctx
            {
                PageTitle = None
                HasSearchBar = true
            }
            [
                div [ Class "container" ] [
                    section [ Class "my-6" ] [
                        div [
                            classes [
                                "is-gap-3"
                                "grid"
                                "recipe-summaries-container"
                            ]
                        ] [
                            for recipe in rcps do
                                div [
                                    classes [ "cell" ]
                                    HtmlProperties.Custom("x-data", "")
                                    HtmlProperties.Custom(
                                        "x-show",
                                        $"!$store.search || '{recipe.Recipe.Name}'.includes($store.search)"
                                    )
                                ] [ Layout.recipeSummary recipe ]
                        ]
                    ]
                ]
            ]


    ctx.TryGetValues<Recipeloader.RecipeEnvelope>()
    |> Option.defaultValue Seq.empty
    |> Seq.sortBy (fun r -> r.Recipe.Name)
    |> layoutForRecipes
    |> Layout.render ctx
    |> Prelude.String.prefix "<!DOCTYPE html>"

let generate (ctx: SiteContents) (_projectRoot: string) (_page: string) =
    generate' ctx
