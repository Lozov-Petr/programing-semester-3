 (******************************
           Лозов Пётр
           Группа 271
            16.10.13
         Красивый курсор
 *******************************)

open System
open System.Windows.Forms
open System.Drawing

type myCursor(startPointCursor : Point) =
    
    let mutable seqPoint = Seq.empty
    let mutable pointCursor = startPointCursor

    let tim = new Timers.Timer(
                                   Interval = 10.0, 
                                   Enabled  = true                              
                              )

    member this.timer = tim
    
    member this.newPointCursor(x : int, y : int) = 
        pointCursor <- new Point(x, y)

    member this.getPointCursor = 
        pointCursor

    member this.update(points : Point * Point) =
        seqPoint <- Seq.truncate 30 <| Seq.append [points] seqPoint

    member this.draw(g : Graphics) =
        let draw (a:Point,b:Point) i =
            use pen = new Pen(Color.Blue, float32 i)
            g.FillEllipse(Brushes.Blue, a.X - i / 2, a.Y - i / 2, i, i)
            g.DrawLine(pen, a, b)
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        Seq.iter2 draw seqPoint [(Seq.length seqPoint)..(-1)..1]

let form = new Form(
                       Text = "GUI and Events",
                       MaximizeBox = false,
                       MinimizeBox = false,
                       FormBorderStyle = FormBorderStyle.Fixed3D,
                       Height = 700,
                       Width = 700,
                       Top = 0,
                       BackColor = Color.WhiteSmoke
                   )

Cursor.Hide()
let cursor = new myCursor(new Point(form.Width / 2, form.Height / 2))

form.MouseMove.Add(fun p -> cursor.newPointCursor(p.X, p.Y))

let eventTime = cursor.timer.Elapsed 
                |> Event.map (fun _ -> cursor.getPointCursor) 
                |> Event.pairwise

eventTime.Add(
                 fun x ->
                     cursor.update(x)
                     form.Invalidate()
             )

form.Paint.Add(
                  fun x -> 
                      cursor.draw(x.Graphics)
              )
    
Application.Run(form)