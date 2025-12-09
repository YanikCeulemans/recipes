#r "../_lib/Fornax.Core.dll"
#if !FORNAX
#load "../prelude.fsx"
#load "../loaders/recipeloader.fsx"
#load "../loaders/pageloader.fsx"
#load "../loaders/globalloader.fsx"
#endif

open Prelude
open Html

let classes cs = String.concat " " cs |> Class

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

let XText (v: string) = HtmlProperties.Custom("x-text", v)
let XData (v: string) = HtmlProperties.Custom("x-data", v)
let XOnClick (v: string) = HtmlProperties.Custom("x-on:click", v)
let XModel (v: string) = HtmlProperties.Custom("x-model", v)
let XShow (v: string) = HtmlProperties.Custom("x-show", v)

type LayoutConfig = {
    PageTitle: string option
    HasSearchBar: bool
}

let layout (ctx: SiteContents) (layoutConfig: LayoutConfig) bodyCnt =
    let ttl =
        ctx.TryGetValue<Globalloader.SiteInfo>()
        |> Option.map (fun si -> si.title)
        |> List.singleton
        |> List.appendWith layoutConfig.PageTitle
        |> List.choose id
        |> String.concat " | "

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
                Href "/favicon-32x32.png"
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
            link [
                Rel "stylesheet"
                Href
                    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/7.0.0/css/all.min.css"
            ]
            link [ Rel "stylesheet"; Type "text/css"; Href "/style/style.css" ]
            script [
                Defer true
                Src
                    "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
            ] []
            script [ Defer true ] [
                !!"""
                document.addEventListener('alpine:init', () => {
                    Alpine.store('search', '');
                });
                """
            ]
        ]
        body [] [
            nav [ Class "navbar" ] [
                div [ Class "container" ] [
                    div [ Class "navbar-brand" ] [
                        a [ Class "navbar-item"; Href "/" ] [ !!"Home" ]
                    ]
                    if layoutConfig.HasSearchBar then
                        div [
                            Class "navbar-item field is-flex-grow-1"
                            XData ""
                        ] [
                            p [
                                Class
                                    "control has-icons-left has-icons-right is-flex-grow-1"
                            ] [
                                input [
                                    Class "input"
                                    Type "search"
                                    Placeholder "Search recipe"
                                    XModel "$store.search"
                                ]
                                span [ Class "icon is-small is-left" ] [
                                    i [ Class "fas fa-magnifying-glass" ] []
                                ]
                                span [
                                    Class "icon is-small is-right is-clickable"
                                    XShow "!!$store.search"
                                    XOnClick "$store.search = ''"
                                ] [ i [ Class "fas fa-close" ] [] ]
                            ]
                        ]
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
        | Some(xtext, amount) -> span [ XText xtext ] [ !! $"%f{amount}" ]

        span [] [ !!unitText ]

    ]

let formatDuration (duration: Recipeloader.Duration) =
    match Recipeloader.Duration.extract duration with
    | LessThanAMinute secs when secs = 1 -> "1 second"
    | LessThanAMinute secs -> $"{secs} seconds"
    | LessThanAnHour mins when mins = 1 -> "1 minute"
    | LessThanAnHour mins -> $"{mins} minutes"
    | other when other.TotalHours = 1 -> "1 hour"
    // TODO: This reads 1.3333333333333 hours at the moment
    | other -> $"{other.TotalHours} hours"

let durationView (duration: Recipeloader.Duration) =
    nav [ Class "level" ] [
        div [ Class "level-item has-text-centered" ] [
            div [] [
                p [ Class "heading" ] [ !!"Total duration" ]
                p [ Class "title" ] [ !!(formatDuration duration) ]
            ]
        ]
    ]

let tagsView (tags: string Set) =
    if Set.isEmpty tags then
        []
    else
        [
            div [ Class "is-flex is-flex-wrap-wrap is-gap-1" ] [
                for tag in tags -> span [ Class "tag" ] [ !!tag ]
            ]
        ]

let recipeSummary (recipeEnvelope: Recipeloader.RecipeEnvelope) =
    let recipe = recipeEnvelope.Recipe

    article [ Class "card full-height is-flex is-flex-direction-column" ] [
        div [ Class "card-image" ] [
            a [ Href recipeEnvelope.Link ] [
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
                                >> Path.modifyFileName (
                                    String.prefix "thumbnail-"
                                )
                            )
                            |> Option.defaultValue (
                                Recipeloader.RecipeImage.format recipe.Image
                            )
                        )
                    ]
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
            h3 [ Class "scaling-size-3 has-text-centered block is-flex-grow-1" ] [
                a [ Href recipeEnvelope.Link ] [ !!recipe.Name ]
            ]
            div [ Class "is-flex is-flex-direction-column is-gap-1" ] [
                span [ Class "icon-text" ] [
                    span [ Class "icon" ] [ i [ Class "fas fa-stopwatch" ] [] ]
                    span [] [ !!(formatDuration recipe.Duration) ]
                ]
                yield!
                    recipe.Tags |> Option.map tagsView |> Option.defaultValue []
            ]
        ]
    ]

let sortIngredients (ingredients: Recipeloader.Ingredient seq) =
    ingredients |> Seq.sortBy Recipeloader.Ingredient.isToTaste

let private ingredientsView
    props
    servingAmount
    (ingredients: Recipeloader.Ingredient seq)
    =
    if Seq.isEmpty ingredients then
        []
    else
        [
            span props [
                for ingredient in ingredients do
                    ingredientView servingAmount ingredient
            ]
        ]

let stepsView servingAmount (steps: Recipeloader.Step seq) =
    if Seq.isEmpty steps then
        []
    else
        [
            div [ Class "column" ] [
                h3 [ Class "is-size-3" ] [ !!"Instructions" ]
                ol [ Class "no-list-style-type" ] [
                    for index, step in Seq.indexed steps ->
                        li [ Class "my-5" ] [
                            div [ Class "is-gap-1 ingredient-grid" ] [
                                yield!
                                    ingredientsView
                                        [
                                            classes [
                                                "is-flex"
                                                "is-flex-direction-row"
                                                "is-flex-wrap-wrap"
                                                "has-text-grey"
                                                "is-size-6"
                                                "is-column-gap-2"
                                                "is-col-start-2"
                                                "cell"
                                            ]
                                        ]
                                        servingAmount
                                        step.Ingredients
                                span [ Class "is-size-5" ] [
                                    !! $"{index + 1}."
                                ]
                                span [ Class "is-size-5" ] [
                                    !!step.Description
                                ]
                            ]
                        ]
                ]
            ]
        ]

let recipeLayout (recipeEnvelope: Recipeloader.RecipeEnvelope) =
    let recipe = recipeEnvelope.Recipe
    let serving = recipe.Instructions.Serving
    let ingredients = Recipeloader.Instructions.ingredients recipe.Instructions

    section [ Class "is-clipped" ] [
        div [ Class "has-text-centered block mt-6" ] [
            p [ Class "title" ] [
                a [ Href recipeEnvelope.Link ] [ !!recipe.Name ]
            ]
            durationView recipe.Duration
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
            div [
                Class "columns mb-6"
                XData("{" + $"serving: {serving}" + "}")
            ] [
                div [ Class "column" ] [
                    nav [ Class "panel" ] [
                        yield
                            p [ Class "panel-heading m-0" ] [ !!"Ingredients" ]
                        yield
                            div [ Class "panel-block" ] [
                                div [ Class "is-size-4 container is-flex" ] [
                                    button [
                                        Class "button level-left"
                                        XOnClick
                                            "serving = Math.max(1, serving - 1)"
                                    ] [ !!"-" ]
                                    span [ Class "level-item is-flex-grow-1" ] [
                                        span [ Class "is-1 is-flex is-gap-1" ] [
                                            span [] [ !! $"Serving: " ]
                                            span [ XText "serving" ] []
                                        ]
                                    ]
                                    button [
                                        Class "button level-right"
                                        XOnClick "serving++"
                                    ] [ !!"+" ]
                                ]
                            ]
                        for ingredient in sortIngredients ingredients ->
                            span [ Class "panel-block" ] [
                                ingredientView serving ingredient
                            ]
                    ]
                ]
                yield!
                    stepsView
                        recipe.Instructions.Serving
                        recipe.Instructions.Steps
            ]
        ]
    ]
