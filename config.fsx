#r "_lib/Fornax.Core.dll"

open Config
open System.IO

let postPredicate (projectRoot: string, page: string) =
    let fileName = Path.Combine(projectRoot, page)
    let ext = Path.GetExtension page

    if ext = ".md" then
        let ctn = File.ReadAllText fileName
        page.Contains "_public" |> not && ctn.Contains "Layout: post"
    else
        false

let recipePredicate (projectRoot: string, page: string) =
    let fileName = Path.Combine(projectRoot, page)
    let ext = Path.GetExtension page

    ext = ".kdl"

let staticPredicate (projectRoot: string, page: string) =
    let ext = Path.GetExtension page

    let fileShouldBeExcluded =
        ext = ".fsx"
        || ext = ".md"
        || ext = ".kdl"
        || page.Contains ".hash-cache"
        || page.Contains "_public"
        || page.Contains "_bin"
        || page.Contains "_lib"
        || page.Contains "_data"
        || page.Contains "_settings"
        || page.Contains "_config.yml"
        || page.Contains ".config"
        || page.Contains ".sass-cache"
        || page.Contains ".git"
        || page.Contains ".ionide"
        || page.Contains ".DS_Store"
        || page.Contains "Session.vim"
        || page.Contains ".editorconfig"
        || page.Contains "mise.toml"

    fileShouldBeExcluded |> not

let excludedFilePathParts = [
    "_public"
    ".git"
    "_bin"
    "_lib"
    "_data"
    "_settings"
]

let recipeImagePredicate (projectRoot: string, page: string) =
    let ext = Path.GetExtension page
    let file = Path.GetFileName page

    List.contains ext [ ".jpg"; ".jpeg"; ".png"; ".webp" ]
    && file.StartsWith "recipe-"
    && not (excludedFilePathParts |> List.exists page.Contains)


let config = {
    Generators = [
        {
            Script = "recipe.fsx"
            Trigger = OnFilePredicate recipePredicate
            OutputFile = ChangeExtension "html"
        }
        {
            Script = "recipeimages.fsx"
            Trigger = OnFilePredicate recipeImagePredicate
            OutputFile = MultipleFiles id
        }
        {
            Script = "staticfile.fsx"
            Trigger = OnFilePredicate staticPredicate
            OutputFile = SameFileName
        }
        {
            Script = "index.fsx"
            Trigger = OnFile "generators/index.fsx"
            OutputFile = NewFileName "index.html"
        }
    ]
}
