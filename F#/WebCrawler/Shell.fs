(******************************
           Лозов Пётр
           Группа 271
            25.12.13
        WebCrawler (Окно)
 *******************************)

module Shell

open WebCrawler
open System.Windows.Forms
open System.Drawing

type ShellForWebCrawler() as this =
    inherit Form(
                    Text            = "Web Crawler",
                    Width           = 420,
                    Height          = 150,
                    FormBorderStyle = FormBorderStyle.Fixed3D,
                    MaximizeBox     = false
                )

    let infoInput = new Label(
                                Left      = 100,
                                Top       = 5,
                                Width     = 200,
                                Text      = "Введите URI:",
                                TextAlign = ContentAlignment.MiddleCenter
                             )
    
    let input = new TextBox(
                                Left  = 50,
                                Top   = 25,
                                Width = 300
                           )

    let start = new Button(
                            Left   = 100,
                            Top    = 55,
                            Width  = 200,
                            Height = 40,
                            Text   = "Запуск"
                          )

    let webCrawler = new WebCrawler()

    do  this.Controls.AddRange([|input; infoInput; start|])
        start.Click.Add(fun _ -> 
                            input.Enabled <- false
                            start.Enabled <- false
                            start.Text <- "Ждите"
                            webCrawler.Crawle(input.Text)
                            input.Enabled <- true
                            start.Enabled <- true
                            start.Text <- "Запуск"
                       )
        
do Application.Run(new ShellForWebCrawler())
