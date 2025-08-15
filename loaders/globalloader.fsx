#r "../_lib/Fornax.Core.dll"

type SiteInfo = { title: string; description: string }

let loader (projectRoot: string) (siteContent: SiteContents) =
    let siteInfo = {
        title = "Our recipe book"
        description = "Recipes we like and use all the time"
    }

    siteContent.Add siteInfo

    siteContent
