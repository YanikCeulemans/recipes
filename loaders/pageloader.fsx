#r "../_lib/Fornax.Core.dll"

type Page = { title: string; link: string }

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add({ title = "Home"; link = "/" })

    siteContent
