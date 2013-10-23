(******************************
           Лозов Пётр
           Группа 271
            24.10.13
          Мой мир (ООП)
 *******************************)

open System
open System.Drawing
open System.Windows.Forms

/////////////////////////////////////////////////////////////////////////////////////////

type MyMath() =
    
    static let rnd = new Random()
    
    static member RndDouble = rnd.NextDouble()

    static member Len(vector : float * float) =
        let (X, Y) = vector
        Math.Sqrt(X * X + Y * Y)
        
    static member Add(vector1 : float * float, vector2 : float * float) =
        let (X1, Y1) = vector1
        let (X2, Y2) = vector2
        (X1 + X2, Y1 + Y2)
        
    static member Mul(f : float, vector : float * float) =
        let (X, Y) = vector
        (f * X, f * Y)

    static member Sub(vector1 : float * float, vector2 : float * float) =
        MyMath.Add(vector1, MyMath.Mul(-1.0, vector2))
        
    static member Norm(vector : float * float) = 
        MyMath.Mul(1.0 / MyMath.Len vector, vector)

    static member float32(vector : float * float) =
        let (X, Y) = vector
        (float32 X, float32 Y)

    static member rndVector(f : float) = ((MyMath.RndDouble - 0.5) * 2.0 * f, (MyMath.RndDouble - 0.5) * 2.0 * f)
         
/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Creature(maxAge : int, maxWeight : int, coordinates : float * float) =

    let maxAge = maxAge
    let maxWeight = maxWeight
    
    let mutable isAlive = true
    let mutable curAge = 0
    let mutable curWeight = maxWeight / 2
    let mutable coordinates = MyMath.Add(coordinates, MyMath.rndVector(200.0))

    member this.IsAlive = isAlive
    member this.MaxAge = maxAge
    member this.MaxWeight = maxWeight

    member this.Coordinates
        with get() = coordinates
        and set(value) = coordinates <- value

    member this.CurWeight 
        with get() = curWeight
        and set(value) = curWeight <- value

    member this.Die = isAlive <- false

    member this.BecomeOlder =
        if isAlive then
            if curAge <= maxAge then curAge <- curAge + 1
            else this.Die

    abstract member WeightChange : unit

    member this.NextMoment =
        if isAlive then
            this.BecomeOlder
            this.WeightChange

    member this.CanProgeny =
        isAlive && curWeight >= maxWeight

    member this.Distance(creature : Creature) =
        MyMath.Len <| MyMath.Sub(creature.Coordinates, this.Coordinates)

    static member SizeWorld = 600.0
        
/////////////////////////////////////////////////////////////////////////////////////////

type Plant(maxAge : int, maxWeight : int, coordinates : float * float) =
    inherit Creature(maxAge, maxWeight, coordinates)

    member this.Coordinates = coordinates

    override this.WeightChange =
        if this.IsAlive then
            this.CurWeight <- this.CurWeight + 1

/////////////////////////////////////////////////////////////////////////////////////////

type Animal(maxAge : int, maxWeight : int, speed : float, coordinates : float * float) =
    inherit Creature(maxAge, maxWeight, coordinates)

    let speed = speed 

    static member EatDistance = 10.0

    override this.WeightChange =
        if this.IsAlive then
            if this.CurWeight > 0 then
                this.CurWeight <- this.CurWeight - 1
            else this.Die

    member this.Run(creature : Creature) = 
        this.Coordinates <- MyMath.Add(this.Coordinates, MyMath.Mul(speed, MyMath.Norm(MyMath.Sub(creature.Coordinates, this.Coordinates)))) 

/////////////////////////////////////////////////////////////////////////////////////////

type Herbivorous(maxAge : int, maxWeight : int, speed : float, coordinates : float * float) =
    inherit Animal(maxAge, maxWeight, speed, coordinates)

    member this.Eat(plant : Plant) =
        if plant.IsAlive && Herbivorous.EatDistance >= MyMath.Len(MyMath.Sub(this.Coordinates, plant.Coordinates)) then
            this.CurWeight <- this.CurWeight + plant.CurWeight
            plant.Die

/////////////////////////////////////////////////////////////////////////////////////////

type Predator(maxAge : int, maxWeight : int, speed : float, coordinates : float * float) =
    inherit Animal(maxAge, maxWeight, speed, coordinates)

    member this.Eat(herbivorous : Herbivorous) =
        if herbivorous.IsAlive && Predator.EatDistance >= MyMath.Len(MyMath.Sub(this.Coordinates, herbivorous.Coordinates)) then
            this.CurWeight <- this.CurWeight + herbivorous.CurWeight
            herbivorous.Die

/////////////////////////////////////////////////////////////////////////////////////////

type Grass(coordinates : float * float) =
    inherit Plant(100, 20, coordinates)
    
    new(size : float) = new Grass((MyMath.RndDouble * size, MyMath.RndDouble * size))

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Grass(Creature.SizeWorld)
        else None

/////////////////////////////////////////////////////////////////////////////////////////

type Sheep(coordinates : float * float) =
    inherit Herbivorous(200, 200, 5.0, coordinates)

    new(size : float) = new Sheep((MyMath.RndDouble *size, MyMath.RndDouble * size))

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Sheep(this.Coordinates)
        else None

/////////////////////////////////////////////////////////////////////////////////////////

type Wolf(coordinates : float * float) =
    inherit Predator(150, 150, 4.0, coordinates)

    new(size : float) = new Wolf((MyMath.RndDouble * size, MyMath.RndDouble * size))

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Wolf(this.Coordinates)
        else None

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

type World(numberCreature : int) =

    let mutable listGrass = []
    let mutable listSheep = []
    let mutable listWolf = []
    let eventTime = (new Timers.Timer(Interval = 10.0, Enabled  = true)).Elapsed

    do
        eventTime.Add(fun _ ->
            
            listGrass <- List.filter(fun (x : Grass) -> x.IsAlive) listGrass
            listSheep <- List.filter(fun (x : Sheep) -> x.IsAlive) listSheep
            listWolf <- List.filter(fun (x : Wolf) -> x.IsAlive) listWolf

            if listGrass = [] then 
                listGrass <- List.map (fun _ -> new Grass(Creature.SizeWorld)) [1..numberCreature * 3]
            if listSheep = [] then
                listSheep <- List.map (fun _ -> new Sheep(Creature.SizeWorld)) [1..numberCreature]
            if listWolf = [] then 
                listWolf <- List.map (fun _ -> new Wolf(Creature.SizeWorld)) [1..numberCreature]

            List.iter  (  
                          fun (grass : Grass) ->
                              if List.length listGrass < numberCreature * 3 then  
                                let progeny = grass.Progeny
                                if progeny.IsSome then listGrass <- progeny.Value :: listGrass
                              grass.NextMoment
            
                       ) listGrass
            List.iter  (
                          fun (sheep : Sheep) ->
                              let grass = List.minBy (fun (grass : Grass) -> grass.Distance(sheep)) listGrass
                              sheep.Run(grass)
                              sheep.Eat(grass)
                              if List.length listSheep < numberCreature * 3 then 
                                let progeny = sheep.Progeny
                                if progeny.IsSome then listSheep <- progeny.Value :: listSheep
                              sheep.NextMoment
                       ) listSheep

            List.iter  (
                          fun (wolf : Wolf) ->
                              let sheep = List.minBy (fun (sheep : Sheep) -> sheep.Distance(wolf)) listSheep
                              wolf.Run(sheep)
                              wolf.Eat(sheep)
                              if List.length listWolf < numberCreature * 3 then 
                                let progeny = wolf.Progeny
                                if progeny.IsSome then listWolf <- progeny.Value :: listWolf
                              wolf.NextMoment
                       ) listWolf
                     )

    member this.ListDrawingGrass = listGrass |> List.map (fun x -> (MyMath.float32 x.Coordinates, float32 x.CurWeight / 10.0f))
    member this.ListDrawingSheep = listSheep |> List.map (fun x -> (MyMath.float32 x.Coordinates, float32 x.CurWeight / 10.0f))
    member this.ListDrawingWolf = listWolf |> List.map (fun x -> (MyMath.float32 x.Coordinates, float32 x.CurWeight / 10.0f))
    member this.EventTime = eventTime

type MyForm(numberCreature : int) as this =
    inherit Form(   
                    Text = "My World",
                    MaximizeBox = false,
                    MinimizeBox = false,
                    FormBorderStyle = FormBorderStyle.Fixed3D,
                    Height = int Creature.SizeWorld + 40,
                    Width = int Creature.SizeWorld + 20,
                    BackColor = Color.WhiteSmoke
                )
    
    let world = new World(numberCreature)
 
    do 
        this.DoubleBuffered <- true

        world.EventTime.Add(fun _ -> this.Invalidate())
        this.Paint.Add(
                        fun e ->
                            let g = e.Graphics
                            g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
                            use green = new SolidBrush(Color.Green)
                            use blue = new SolidBrush(Color.Blue)
                            use red = new SolidBrush(Color.Red)
                            List.iter (fun ((X, Y), R) -> g.FillEllipse(green, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingGrass
                            List.iter (fun ((X, Y), R) -> g.FillEllipse(blue, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingSheep
                            List.iter (fun ((X, Y), R) -> g.FillEllipse(red, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingWolf
                      )    

Application.Run(new MyForm(10))S