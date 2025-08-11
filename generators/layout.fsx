#r "../_lib/Fornax.Core.dll"
#if !FORNAX
#load "../prelude.fsx"
#load "../loaders/recipeloader.fsx"
#load "../loaders/pageloader.fsx"
#load "../loaders/globalloader.fsx"
#endif

open Prelude
open Html

let injectWebsocketCode (webpage: string) =
    let websocketScript =
        """
        <script type="text/javascript">
          var wsUri = "ws://localhost:8080/websocket";
      function init()
      {
        websocket = new WebSocket(wsUri);
        websocket.onclose = function(evt) { onClose(evt) };
      }
      function onClose(evt)
      {
        console.log('closing');
        websocket.close();
        document.location.reload();
      }
      window.addEventListener("load", init, false);
      </script>
        """

    let head = "<head>"
    let index = webpage.IndexOf head
    webpage.Insert(index + head.Length + 1, websocketScript)

let layout (ctx: SiteContents) active bodyCnt =
    let pages =
        ctx.TryGetValues<Pageloader.Page>() |> Option.defaultValue Seq.empty

    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>()

    let ttl =
        siteInfo |> Option.map (fun si -> si.title) |> Option.defaultValue ""

    let menuEntries =
        pages
        |> Seq.map (fun p ->
            let cls =
                if p.title = active then
                    "navbar-item is-active"
                else
                    "navbar-item"

            a [ Class cls; Href p.link ] [ !!p.title ]
        )
        |> Seq.toList

    html [] [
        head [] [
            meta [ CharSet "utf-8" ]
            meta [
                Name "viewport"
                Content "width=device-width, initial-scale=1"
            ]
            title [] [ !!ttl ]
            link [
                Rel "icon"
                Type "image/png"
                Sizes "32x32"
                Href "/images/favicon.png"
            ]
            link [
                Rel "stylesheet"
                Href
                    "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
            ]
            link [
                Rel "stylesheet"
                Href "https://fonts.googleapis.com/css?family=Open+Sans"
            ]
            link [
                Rel "stylesheet"
                Href
                    "https://cdn.jsdelivr.net/npm/bulma@1.0.4/css/bulma.min.css"
            ]
            link [ Rel "stylesheet"; Type "text/css"; Href "/style/style.css" ]
            script [
                Defer true
                Src
                    "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
            ] []

        ]
        body [] [
            nav [ Class "navbar" ] [
                div [ Class "container" ] [
                    div [ Class "navbar-brand" ] [
                        span [
                            Class "navbar-burger burger"
                            HtmlProperties.Custom("data-target", "navbarMenu")
                        ] [ span [] []; span [] []; span [] [] ]
                    ]
                    div [ Id "navbarMenu"; Class "navbar-menu" ] menuEntries
                ]
            ]
            yield! bodyCnt
        ]
    ]

let render (ctx: SiteContents) cnt =
    let disableLiveRefresh = true

    cnt
    |> HtmlElement.ToString
    |> fun n -> if disableLiveRefresh then n else injectWebsocketCode n

let XText (v: string) = HtmlProperties.Custom("x-text", v)
let XData (v: string) = HtmlProperties.Custom("x-data", v)
let XOnClick (v: string) = HtmlProperties.Custom("x-on:click", v)

let private ingredientView
    (servingAmount: int)
    (ingredient: Recipeloader.Ingredient)
    =
    let name =
        [
            ingredient.Name |> Prelude.String.captilize
            match ingredient.Variant |> Option.map Prelude.String.braced with
            | None -> ()
            | Some x -> x
        ]
        |> String.concat " "

    let xtext, unitText =
        Recipeloader.IngredientAmount.formatWithScaling
            $"serving / %d{servingAmount}"
            ingredient.Amount

    span [] [
        span [] [ !! $"{name}: " ]
        match xtext with
        | None -> ()
        | Some(xtext, amount) -> span [ XText xtext ] [ !! $"%d{amount}" ]

        span [] [ !!unitText ]

    ]

let keyInfoView (recipe: Recipeloader.Recipe) = [
    match recipe.KeyInfo with
    | None -> ()
    | Some keyInfo ->
        nav [ Class "level" ] [
            for KeyValue(key, value) in keyInfo ->
                div [ Class "level-item has-text-centered" ] [
                    div [] [
                        p [ Class "heading" ] [ !!key ]
                        p [ Class "title" ] [ !!value ]
                    ]
                ]
        ]
]

let recipeSummary (recipeEnvelope: Recipeloader.RecipeEnvelope) =
    let recipe = recipeEnvelope.Recipe

    article [ Class "card full-height is-flex is-flex-direction-column" ] [
        div [ Class "card-image" ] [
            figure [ Class "image is-16by9" ] [
                img [
                    Src(
                        recipe.Image
                        |> Some
                        |> Option.filter (
                            Recipeloader.RecipeImage.isExternalImage >> not
                        )
                        |> Option.map (
                            Recipeloader.RecipeImage.format
                            >> Path.modifyExt (always ".webp")
                            >> Path.modifyFileName (String.prefix "thumbnail-")
                        )
                        |> Option.defaultValue (
                            Recipeloader.RecipeImage.format recipe.Image
                        )
                    )
                ]
            ]
        ]
        div [
            Class
                "card-content \
                is-flex-grow-1 \
                is-flex \
                is-flex-direction-column \
                justify-between"
        ] [
            h3 [ Class "is-size-3 has-text-centered block" ] [
                a [ Href recipeEnvelope.Link ] [ !!recipe.Name ]
            ]
            yield! keyInfoView recipe
        ]
    ]

let sortIngredients (ingredients: Recipeloader.Ingredient seq) =
    ingredients |> Seq.sortBy Recipeloader.Ingredient.isToTaste

let recipeLayout (recipeEnvelope: Recipeloader.RecipeEnvelope) =
    let recipe = recipeEnvelope.Recipe

    div [] [
        div [ Class "media-content has-text-centered block" ] [
            p [ Class "title" ] [
                a [ Href recipeEnvelope.Link ] [ !!recipe.Name ]
            ]
            yield! keyInfoView recipe
        ]
        div [ Class "content article-body" ] [
            div [ Class "block" ] [
                figure [ Class "image is-16by9" ] [
                    img [
                        Src(
                            recipe.Image
                            |> Some
                            |> Option.filter (
                                Recipeloader.RecipeImage.isExternalImage >> not
                            )
                            |> Option.map (
                                Recipeloader.RecipeImage.format
                                >> Path.modifyExt (always ".webp")
                            )
                            |> Option.defaultValue (
                                Recipeloader.RecipeImage.format recipe.Image
                            )
                        )
                    ]
                ]
            ]
            div [ Class "columns" ] [
                div [ Class "column" ] [
                    nav [
                        Class "panel"
                        XData(
                            "{" + $"serving: {recipe.Ingredients.Serving}" + "}"
                        )
                    ] [
                        yield
                            p [ Class "panel-heading m-0" ] [ !!"Ingredients" ]
                        yield
                            div [ Class "panel-block" ] [
                                div [ Class "is-size-4 container level" ] [
                                    button [
                                        Class "button level-left"
                                        XOnClick
                                            "serving = Math.max(1, serving - 1)"
                                    ] [ !!"-" ]
                                    span [ Class "level-item" ] [
                                        span [ Class "columns is-1" ] [
                                            span [ Class "column" ] [
                                                !! $"Serving: "
                                            ]
                                            span [
                                                Class "column"
                                                XText "serving"
                                            ] []
                                        ]
                                    ]
                                    button [
                                        Class "button level-right"
                                        XOnClick "serving++"
                                    ] [ !!"+" ]
                                ]
                            ]
                        for ingredient in
                            sortIngredients recipe.Ingredients.Ingredients ->
                            span [ Class "panel-block" ] [
                                ingredientView
                                    recipe.Ingredients.Serving
                                    ingredient
                            ]
                    ]
                ]
                match recipe.Instructions with
                | [||] -> ()
                | instructions ->
                    div [ Class "column" ] [
                        h3 [ Class "is-size-3" ] [ !!"Instructions" ]
                        ol [] [
                            for instruction in instructions ->
                                li [ Class "my-5" ] [
                                    span [] [ !!instruction ]
                                ]
                        ]
                    ]
            ]
        ]
    ]
