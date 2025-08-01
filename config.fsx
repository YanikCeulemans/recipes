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
// if ext = ".kdl" then
//     let ctn = File.ReadAllText fileName
//     page.Contains "_public" |> not && ctn.Contains "Layout: post"
// else
//     false

let staticPredicate (projectRoot: string, page: string) =
    let ext = Path.GetExtension page

    let fileShouldBeExcluded =
        ext = ".fsx"
        || ext = ".md"
        || ext = ".kdl"
        || page.Contains "_public"
        || page.Contains "_bin"
        || page.Contains "_lib"
        || page.Contains "_data"
        || page.Contains "_settings"
        || page.Contains "_config.yml"
        || page.Contains ".sass-cache"
        || page.Contains ".git"
        || page.Contains ".ionide"
        || page.Contains ".DS_Store"
        || page.Contains "Session.vim"
        || page.Contains ".editorconfig"
        || page.Contains "mise.toml"

    fileShouldBeExcluded |> not


let config = {
    Generators = [
        {
            Script = "less.fsx"
            Trigger = OnFileExt ".less"
            OutputFile = ChangeExtension "css"
        }
        {
            Script = "sass.fsx"
            Trigger = OnFileExt ".scss"
            OutputFile = ChangeExtension "css"
        }
        {
            Script = "post.fsx"
            Trigger = OnFilePredicate postPredicate
            OutputFile = ChangeExtension "html"
        }
        {
            Script = "recipe.fsx"
            Trigger = OnFilePredicate recipePredicate
            OutputFile = ChangeExtension "html"
        }
        {
            Script = "staticfile.fsx"
            Trigger = OnFilePredicate staticPredicate
            OutputFile = SameFileName
        }
        {
            Script = "index.fsx"
            Trigger = Once
            OutputFile = MultipleFiles id
        }
        {
            Script = "about.fsx"
            Trigger = Once
            OutputFile = NewFileName "about.html"
        }
        {
            Script = "contact.fsx"
            Trigger = Once
            OutputFile = NewFileName "contact.html"
        }
    ]
}
