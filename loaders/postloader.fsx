#r "../_lib/Fornax.Core.dll"
#r "../_lib/Markdig.dll"
#r "nuget: YamlDotNet"

open System
open System.IO
open Markdig
open Markdig.Syntax
open Markdig.Extensions.Yaml
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

type PostConfig = { disableLiveRefresh: bool }

type Post = {
    file: string
    link: string
    title: string
    author: string option
    published: System.DateOnly option
    tags: string list
    content: string
    summary: string
}

let contentDir = "posts"

let markdownPipeline =
    MarkdownPipelineBuilder()
        .UsePipeTables()
        .UseGridTables()
        .UseYamlFrontMatter()
        .Build()

let isSeparator (input: string) = input.StartsWith "---"

let isSummarySeparator (input: string) = input.Contains "<!--more-->"

[<CLIMutable>]
type PostMetadata = {
    Layout: string
    Title: string
    Author: string option
    Published: DateOnly option
    /// can be null due to deserialisation
    Tags: string array
    TotalTime: string option
}

let yamlDeserializer =
    DeserializerBuilder()
        .WithNamingConvention(CamelCaseNamingConvention.Instance)
        .Build()

///`fileContent` - content of page to parse. Usually whole content of `.md` file
///returns content of config that should be used for the page
let getConfig (fileContent: string) : PostMetadata =
    let document = Markdown.Parse(fileContent, markdownPipeline)
    let b = document.Descendants<YamlFrontMatterBlock>() |> Seq.head
    let frontMatterText = $"---\n{b.Lines.ToString()}\n..."

    try
        yamlDeserializer.Deserialize<PostMetadata> frontMatterText
    with e ->
        exn ("could not deserialize frontmatter as yaml", e) |> raise

(*
    let fileContent = fileContent.Split '\n'
    let fileContent = fileContent |> Array.skip 1 //First line must be ---
    let indexOfSeperator = fileContent |> Array.findIndex isSeparator

    let splitKey (line: string) =
        let seperatorIndex = line.IndexOf(':')

        if seperatorIndex > 0 then
            let key = line.[.. seperatorIndex - 1].Trim().ToLower()
            let value = line.[seperatorIndex + 1 ..].Trim()
            Some(key, value)
        else
            None

    fileContent
    |> Array.splitAt indexOfSeperator
    |> fst
    |> Seq.choose splitKey
    |> Map.ofSeq
    *)

///`fileContent` - content of page to parse. Usually whole content of `.md` file
///returns HTML version of content of the page
let getContent (fileContent: string) =
    let fileContent = fileContent.Split '\n'
    let fileContent = fileContent |> Array.skip 1 //First line must be ---
    let indexOfSeperator = fileContent |> Array.findIndex isSeparator
    let _, content = fileContent |> Array.splitAt indexOfSeperator

    let summary, content =
        match content |> Array.tryFindIndex isSummarySeparator with
        | Some indexOfSummary ->
            let summary, _ = content |> Array.splitAt indexOfSummary
            summary, content
        | None -> content, content

    let summary = summary |> Array.skip 1 |> String.concat "\n"
    let content = content |> Array.skip 1 |> String.concat "\n"

    Markdown.ToHtml(summary, markdownPipeline),
    Markdown.ToHtml(content, markdownPipeline)

let trimString (str: string) = str.Trim().TrimEnd('"').TrimStart('"')

let loadFile (rootDir: string) (n: string) =
    let text = File.ReadAllText n

    let config = getConfig text
    let summary, content = getContent text

    let chopLength =
        if rootDir.EndsWith(Path.DirectorySeparatorChar) then
            rootDir.Length
        else
            rootDir.Length + 1

    let dirPart = n |> Path.GetDirectoryName |> (fun x -> x.[chopLength..])

    let file =
        Path
            .Combine(dirPart, (n |> Path.GetFileNameWithoutExtension) + ".md")
            .Replace("\\", "/")

    let link =
        "/"
        + Path
            .Combine(dirPart, (n |> Path.GetFileNameWithoutExtension) + ".html")
            .Replace("\\", "/")

    let title = config.Title |> trimString
    let author = config.Author |> Option.map trimString

    let published = config.Published

    let tags =
        config.Tags
        |> Option.ofObj
        |> Option.map List.ofArray
        |> Option.defaultValue []

    {
        file = file
        link = link
        title = title
        author = author
        published = published
        tags = tags
        content = content
        summary = summary
    }

let loader (projectRoot: string) (siteContent: SiteContents) =
    let postsPath = Path.Combine(projectRoot, contentDir)
    let options = EnumerationOptions(RecurseSubdirectories = true)
    let files = Directory.GetFiles(postsPath, "*", options)

    files
    |> Array.filter (fun n -> n.EndsWith ".md")
    |> Array.map (loadFile projectRoot)
    |> Array.iter siteContent.Add

    siteContent.Add({ disableLiveRefresh = false })
    siteContent
