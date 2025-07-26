#r "../_lib/Fornax.Core.dll"
#if !FORNAX
#load "../loaders/recipeloader.fsx"
#load "../loaders/postloader.fsx"
#load "../loaders/pageloader.fsx"
#load "../loaders/globalloader.fsx"
#endif

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

        ]
        body [] [
            nav [ Class "navbar" ] [
                div [ Class "container" ] [
                    div [ Class "navbar-brand" ] [
                        a [ Class "navbar-item"; Href "/" ] [
                            img [ Src "/images/bulma.png"; Alt "Logo" ]
                        ]
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
    let disableLiveRefresh =
        ctx.TryGetValue<Postloader.PostConfig>()
        |> Option.map (fun n -> n.disableLiveRefresh)
        |> Option.defaultValue false

    cnt
    |> HtmlElement.ToString
    |> fun n -> if disableLiveRefresh then n else injectWebsocketCode n

let published (post: Postloader.Post) =
    post.Metadata.Published
    |> Option.bind (fun dtOnlyText ->
        match System.DateOnly.TryParse dtOnlyText with
        | true, dateOnly -> Some dateOnly
        | false, _ -> None
    )
    |> Option.defaultValue (System.DateOnly.FromDateTime System.DateTime.Now)
    |> fun n -> n.ToString "yyyy-MM-dd"

let postLayout (useSummary: bool) (post: Postloader.Post) =
    let props = post.Metadata.Props |> Option.defaultValue []
    let tags = post.Metadata.Tags |> Option.defaultValue []

    div [ Class "card article" ] [
        div [ Class "card-content" ] [
            div [ Class "media-content has-text-centered" ] [
                p [ Class "title article-title" ] [
                    a [ Href post.Link ] [ !!post.Metadata.Title ]
                ]
                p [ Class "subtitle is-6 article-subtitle" ] [
                    a [ Href "#" ] [ !!(defaultArg post.Metadata.Author "") ]
                    !!(sprintf "on %s" (published post))
                ]
                match props with
                | [] -> ()
                | props ->
                    nav [ Class "level" ] [
                        for prop in props ->
                            div [ Class "level-item has-text-centered" ] [
                                div [] [
                                    p [ Class "heading" ] [ !!prop.Key ]
                                    p [ Class "title" ] [ !!prop.Value ]
                                ]
                            ]
                    ]
                match tags with
                | [] -> ()
                | tags ->
                    div [ Class "article-tags is-flex gap-3" ] [
                        for tag in tags -> span [ Class "tag" ] [ !!tag ]
                    ]
            ]
            div [ Class "content article-body" ] [
                !!(if useSummary then post.Summary else post.Content)

            ]
        ]
    ]

let recipeLayout (recipe: Recipeloader.Recipe) =
    let ingredientUnitView u =
        !!(Recipeloader.IngredientUnit.format u)

    div [ Class "card article" ] [
        div [ Class "card-content" ] [
            div [ Class "media-content has-text-centered" ] [
                p [ Class "title article-title" ] [
                    a [ Href "#todo" ] [ !!recipe.Name ]
                ]
                p [ Class "subtitle is-6 article-subtitle" ] [
                    a [ Href "#" ] [ !!"@yanikc" ]
                ]
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
            div [ Class "content article-body" ] [
                p [] [
                    span [] [ !!"serving: " ]
                    span [] [ !! $"{recipe.Ingredients.Serving}" ]
                ]
                match recipe.Ingredients.Ingredients |> Array.ofSeq with
                | [||] -> ()
                | ingredients ->
                    ul [] [
                        for ingredient in ingredients ->
                            li [] [
                                span [] [
                                    !!ingredient.Name
                                    match ingredient.Variant with
                                    | None -> ()
                                    | Some v -> !! $" ({v})"
                                    !!":"
                                    !! $"{ingredient.Amount}"
                                    ingredientUnitView ingredient.Unit
                                ]
                            ]
                    ]
                match recipe.Instructions with
                | [||] -> ()
                | instructions ->
                    ol [] [
                        for instruction in instructions ->
                            li [] [ span [] [ !!instruction ] ]
                    ]
            ]
        ]
    ]
