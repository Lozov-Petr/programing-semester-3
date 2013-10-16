 (******************************
           Лозов Пётр
           Группа 271
            16.10.13
         Красивый курсор
 *******************************)

open System
open System.Windows.Forms
open System.Drawing

let form = new Form(
                        Text = "GUI and Events",
                        MaximizeBox = false,
                        MinimizeBox = false,
                        FormBorderStyle = FormBorderStyle.Fixed3D,
                        Height = 700,
                        Width = 700,
                        BackColor = Color.WhiteSmoke
                   )

type Cursor() =
    let mutable seqPoint = Seq.empty
    let mutable pointCursor = new Point(form.Width / 2, form.Height / 2)

    member this.newPointCursor(x : int, y : int) = 
        pointCursor <- new Point(x, y)

    member this.getPointCursor = 
        pointCursor

    member this.update(points : Point * Point) =
        seqPoint <- Seq.truncate 20 <| Seq.append [points] seqPoint

    member this.draw(g : Graphics) =
        let draw (a:Point,b:Point) i =
            use pen = new Pen(Color.Blue, float32 i)
            g.FillEllipse(Brushes.Blue, a.X - i / 2, a.Y - i / 2, i, i)
            g.DrawLine(pen, a, b)
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        Seq.iter2 draw seqPoint [(Seq.length seqPoint)..(-1)..1]

let cursor = new Cursor()
let mutable canNext = true

form.MouseMove.Add(fun p -> cursor.newPointCursor(p.X, p.Y))

let timer = new Timers.Timer(10.0)
timer.Start()
let eventTime = timer.Elapsed 
                |> Event.filter (fun _ -> canNext) 
                |> Event.map (fun _ -> cursor.getPointCursor) 
                |> Event.pairwise

eventTime.Add(fun x ->
    canNext <- false
    cursor.update(x)
    form.Invalidate()
    )

form.Paint.Add(fun x ->
    cursor.draw(x.Graphics)
    canNext <- true
    )
    
Application.Run(form)
