module WebCrawler

open Microsoft.FSharp.Control.WebExtensions
open System
open System.IO
open System.Net
open System.Collections.Concurrent
open System.Text.RegularExpressions

type WebCrawler() =
    
    let attendedLinks = new ConcurrentDictionary<string, unit>()
    let downloadedPictures = new ConcurrentDictionary<string, unit>()
                
    static let extensions = "\.(:?ico|bmp|jpeg|jpg|png|gif)"
    static let name       = sprintf "[a-zA-Z0-9._-]+%s" extensions
    static let regUri     = "(:?https?\:)?[a-zA-Z0-9./_-]+"
    static let regPic     = sprintf "(:?(:?href=\")?(?<pic>%s%s))" regUri extensions
    static let regLink    = sprintf "(:?href=\"(?<link>%s)\")" regUri
    static let regAll     = sprintf "%s|%s" regPic regLink
    static let regex      = new Regex(regAll)
    static let regexName  = new Regex(name)

    member private x.GetHTML(link : string) = 
        async {
                try let! html = new Uri(link) |> (new WebClient()).AsyncDownloadString  in return html
                with | _ -> return String.Empty
              } 

    member private x.GetLinks(link : string) (host : string) (html : string) =
        async {
                let res = regex.Matches(html)
                let get (s : string) = 
                    [for l in [for i in res -> i.Groups.[s]] -> l.Value]
                    |> List.filter ((<>) String.Empty) 
                    |> List.map (fun s -> if s.StartsWith "/" then sprintf "%s%s" host s else s)
                let filterLinks = List.filter (fun (s : string) -> s.IndexOf link <> -1)
                return "link" |> get |> filterLinks, "pic" |> get
              }

    member private x.DownloadPicture(link : string) =
        let createName (link : string) = link.Replace("/", " ").Replace(":", " ")
        async {
                try if not <| downloadedPictures.ContainsKey(link) then
                        let uri = new Uri(link)
                        downloadedPictures.GetOrAdd(link, ())
                        let name = regexName.Match(uri.OriginalString).Value
                        (new WebClient()).DownloadFileAsync(uri, sprintf "Picture/%s" <| createName link)
                with _ -> ()
              }

    member private x.PrivateCrawle (host : string) (link : string) =
        async {
                let removeAttendedLinks = List.filter (not << attendedLinks.ContainsKey)
                attendedLinks.GetOrAdd(link, ())
                let! html = x.GetHTML link
                let! links, pics = x.GetLinks link host html

                pics
                |> List.map x.DownloadPicture
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

                links
                |> removeAttendedLinks
                |> List.map (x.PrivateCrawle host)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore
              }

    member x.Crawle(link : string) =
        try let uri = new Uri(link)  
            attendedLinks.Clear()
            downloadedPictures.Clear()
            Directory.CreateDirectory("Picture") |> ignore
            let host = sprintf "%s://%s" uri.Scheme uri.Host

            x.PrivateCrawle host link
            |> Async.RunSynchronously
        
        with _ -> ()