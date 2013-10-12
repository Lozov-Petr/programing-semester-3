open System
open System.Windows.Forms
open System.Drawing

let form = new Form(
                        Text = "GUI and Events",
                        MaximizeBox = false,
                        MinimizeBox = false,
                        Height = 500,
                        Width = 500,
                        BackColor = Color.WhiteSmoke
                   )

let graphics = form.CreateGraphics(SmoothingMode = Drawing2D.SmoothingMode.HighQuality)
let mutable pointMouse = new Point(form.Width / 2, form.Height / 2)
form.MouseMove.Add(fun p -> pointMouse <- new Point(p.X, p.Y))

let mutable listPoint = []

let addPointToList list points =
    
    let rec take list n =
        match list with
        | x::xs when n > 0 -> x::(take xs (n - 1))
        | _ -> []
    
    points::(take list 19)

let timer = new Timers.Timer(4.0)
timer.Start()
let eventTime = timer.Elapsed |> Event.map (fun _ -> pointMouse) |> Event.pairwise

eventTime.Add(fun x ->
    let draw (a:Point,b:Point) i =
        graphics.DrawLine(new Pen(Color.Blue, float32 i), a, b)
        graphics.FillEllipse(Brushes.Blue, a.X - i / 2, a.Y - i / 2, i, i)
    listPoint <- addPointToList listPoint x
    graphics.Clear(form.BackColor)
    List.iter2 draw listPoint [for i in [(listPoint.Length)..(-1)..1] -> i])


Application.Run(form)