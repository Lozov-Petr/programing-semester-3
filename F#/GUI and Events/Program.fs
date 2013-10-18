 (******************************
           Лозов Пётр
           Группа 271
            18.10.13
         Красивый курсор
 *******************************)

open System
open System.Windows.Forms
open System.Drawing

type BeautifulCursor(container : ContainerControl, size : float32, length : int) =
    
    let mutable color = (0, 0, 0)

    let mutable pointCursor = 
        new PointF(float32 <| container.Width / 2, float32 <| container.Height / 2)
    
    let mutable seqLines = 
        let startLine = (pointCursor, pointCursor), color
        seq {for i in 1..length -> startLine}

    let size = 
        if size > 0.0f then size else failwith "Invalid value Size."
 
    let length = 
        if length > 0 then length else failwith "Invalid value Length."
        
    let update = 
        let timer = new Timers.Timer(
                                        Interval = 10.0, 
                                        Enabled  = true                              
                                    )
        let event = timer.Elapsed
                    |> Event.map (fun _ -> pointCursor) 
                    |> Event.pairwise
                    |> Event.map (fun x -> (x, color))

        event.Add(
                  fun x ->
                      let nextColor color =
                          match color with
                          | ( 0 , 0 , b ) when b < 255 -> ( 0 , 0 ,b+5)
                          | ( 0 , g ,255) when g < 255 -> ( 0 ,g+5,255)
                          | ( 0 ,255, b ) when b >  0  -> ( 0 ,255,b-5)
                          | ( r ,255, 0 ) when r < 255 -> (r+5,255, 0 )
                          | (255,255, b ) when b < 255 -> (255,255,b+5)
                          | (255, g ,255) when g >  0  -> (255,g-5,255)
                          | (255, 0 , b ) when b >  0  -> (255, 0 ,b-5)
                          | ( r , 0 , 0 ) when r >  0  -> (r-5, 0 , 0 )
                          | _ -> color

                      seqLines <- Seq.truncate length <| Seq.append [x] seqLines
                      color <- nextColor color
                      container.Invalidate()
                 )
    
    let hideCursor =
        container.MouseEnter.Add(fun _ -> Cursor.Hide())

    let showCursor =
        container.MouseLeave.Add(fun _ -> Cursor.Show())
        
    let moveCursor =
        container.MouseMove.Add(fun p -> pointCursor <- new PointF(float32 p.X, float32 p.Y))

    let draw =
        container.Paint.Add(
                                fun x -> 
                                    let draw ((p1:PointF,p2:PointF),(r,g,b)) i =
                                        use pen = new Pen(Color.FromArgb(r,g,b), i)
                                        use brush = new SolidBrush(Color.FromArgb(r,g,b))
                                        x.Graphics.FillEllipse(brush, p1.X - i / 2.0f, p1.Y - i / 2.0f, i, i)
                                        x.Graphics.DrawLine(pen, p1, p2)
                                    x.Graphics.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
                                    Seq.iter2 draw (List.rev <| List.ofSeq seqLines) 
                                        <| List.map (fun x -> float32 x * size / float32 length) [1..length]
                           )
              
type myForm() as this =        
    inherit Form(
                    Text = "GUI and Events",
                    MaximizeBox = false,
                    MinimizeBox = false,
                    FormBorderStyle = FormBorderStyle.Fixed3D,
                    Height = 700,
                    Width = 700,
                    BackColor = Color.WhiteSmoke                                    
                )

    let doDoubleBuffered = this.DoubleBuffered <- true
    
Application.Run(
                    let form = new myForm()
                    let cursor = new BeautifulCursor(form, 20.0f, 20)
                    form
               )