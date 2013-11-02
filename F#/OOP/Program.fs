open System
open System.Drawing
open System.Windows
open System.Windows.Forms
         
/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Creature(maxAge : int, maxWeight : int, coordinates : Vector) =

    //Размер мира общий для всех существ
    static let mutable sizeWorld = 600
    static let rnd = new Random()

    let maxAge = maxAge
    let maxWeight = maxWeight
    
    let mutable isAlive = true
    let mutable curAge = 0
    let mutable curWeight = maxWeight / 2
    let mutable coordinates = 
        coordinates + new Vector(rnd.NextDouble() * 200.0 - 100.0, rnd.NextDouble() * 200.0 - 100.0)

    static member SizeWorld 
        with get() = sizeWorld
        and set(value) = sizeWorld <- value

    static member RandomFloat = rnd.NextDouble()

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

    // Изменение веса с течением времени для животных и растений происходит по-разному
    abstract member WeightChange : unit

    member this.NextMoment =
        if isAlive then
            this.BecomeOlder
            this.WeightChange

    member this.CanProgeny =
        isAlive && curWeight >= maxWeight

    member this.Distance(creature : Creature) =
        (creature.Coordinates - this.Coordinates).Length

/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Plant(maxAge : int, maxWeight : int, coordinates : Vector) =
    inherit Creature(maxAge, maxWeight, coordinates)

    // С течением времени растение увеличивается
    override this.WeightChange =
        if this.IsAlive then
            this.CurWeight <- this.CurWeight + 1

/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Animal(maxAge : int, maxWeight : int, speed : float, coordinates : Vector) =
    inherit Creature(maxAge, maxWeight, coordinates)

    let speed = speed 
    
    // Дистанция, на которой можно есть
    static member EatDistance = 10.0

    //С течением времени животное уменьшается
    override this.WeightChange =
        if this.IsAlive then
            if this.CurWeight > 0 then
                this.CurWeight <- this.CurWeight - 1
            else this.Die

    member this.Run(creature : Creature) = 
        let mutable vector = creature.Coordinates - this.Coordinates
        vector.Normalize()
        this.Coordinates <-  speed * vector + this.Coordinates

/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Herbivorous(maxAge : int, maxWeight : int, speed : float, coordinates : Vector) =
    inherit Animal(maxAge, maxWeight, speed, coordinates)

    member this.Eat(plant : Plant) =
        if plant.IsAlive && this.Distance(plant) <= Animal.EatDistance then
            this.CurWeight <- this.CurWeight + plant.CurWeight
            plant.Die

/////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Predator(maxAge : int, maxWeight : int, speed : float, coordinates : Vector) =
    inherit Animal(maxAge, maxWeight, speed, coordinates)

    member this.Eat(herbivorous : Herbivorous) =
        if herbivorous.IsAlive && this.Distance(herbivorous) <= Animal.EatDistance then
            this.CurWeight <- this.CurWeight + herbivorous.CurWeight
            herbivorous.Die

/////////////////////////////////////////////////////////////////////////////////////////

type Grass(coordinates : Vector) =
    
    // Максимальный возраст травы - 100
    // Максимальный вес травы     -  20
    inherit Plant(100, 20, coordinates)
    
    new() = new Grass(
                        let sizeV = float Creature.SizeWorld
                        new Vector(Creature.RandomFloat * sizeV, Creature.RandomFloat * sizeV)
                     )

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Grass()
        else None

/////////////////////////////////////////////////////////////////////////////////////////

type Sheep(coordinates : Vector) =
    
    // Максимальный возраст волка - 200
    // Максимальный вес волка     - 200
    // Скорость волка             - 5.0
    inherit Herbivorous(200, 200, 5.0, coordinates)

    new() = new Sheep(
                        let sizeV = float Creature.SizeWorld
                        new Vector(Creature.RandomFloat * sizeV, Creature.RandomFloat * sizeV)
                     )

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Sheep(this.Coordinates)
        else None

/////////////////////////////////////////////////////////////////////////////////////////

type Wolf(coordinates : Vector) =

    // Максимальный возраст волка - 150
    // Максимальный вес волка     - 150
    // Скорость волка             - 4.0
    inherit Predator(150, 150, 4.0, coordinates)

    new() = new Wolf(
                        let sizeV = float Creature.SizeWorld
                        new Vector(Creature.RandomFloat * sizeV, Creature.RandomFloat * sizeV)
                    )

    member this.Progeny =
        if this.CanProgeny then
            this.CurWeight <- this.CurWeight - this.MaxWeight / 2
            Some <| new Wolf(this.Coordinates)
        else None

/////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// ТЕСТЫ /////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

type World(numberCreature : int, sizeWorld : int) =

    let mutable listGrasses = []
    let mutable listSheep = []
    let mutable listWolfs = []
    
    let eventTime = (new Timers.Timer(Interval = 10.0, Enabled = true)).Elapsed

    do
        Creature.SizeWorld <- sizeWorld
        eventTime.Add(fun _ ->
            
            listGrasses <- List.filter(fun (x : Grass) -> x.IsAlive) listGrasses
            listSheep <- List.filter(fun (x : Sheep) -> x.IsAlive) listSheep
            listWolfs <- List.filter(fun (x : Wolf) -> x.IsAlive) listWolfs

            if listGrasses = [] then 
                listGrasses <- List.map (fun _ -> new Grass()) [1..numberCreature]
            if listSheep = [] then
                listSheep <- List.map (fun _ -> new Sheep()) [1..numberCreature]
            if listWolfs = [] then 
                listWolfs <- List.map (fun _ -> new Wolf()) [1..numberCreature]

            List.iter  (  
                          fun (grass : Grass) ->
                              if List.length listGrasses < numberCreature * 3 then  
                                let progeny = grass.Progeny
                                if progeny.IsSome then listGrasses <- progeny.Value :: listGrasses
                              grass.NextMoment
                       ) listGrasses

            List.iter  (
                          fun (sheep : Sheep) ->
                              let grass = List.minBy (fun (grass : Grass) -> grass.Distance(sheep)) listGrasses
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
                              if List.length listWolfs < numberCreature * 3 then 
                                let progeny = wolf.Progeny
                                if progeny.IsSome then listWolfs <- progeny.Value :: listWolfs
                              wolf.NextMoment
                       ) listWolfs
                     )

    member this.ListDrawingGrass = 
            listGrasses 
            |> List.map (fun x -> (float32 x.Coordinates.X, float32 x.Coordinates.Y, float32 x.CurWeight / 10.0f))
    member this.ListDrawingSheep = 
            listSheep 
            |> List.map (fun x -> (float32 x.Coordinates.X, float32 x.Coordinates.Y, float32 x.CurWeight / 10.0f))
    member this.ListDrawingWolf = 
            listWolfs 
            |> List.map (fun x -> (float32 x.Coordinates.X, float32 x.Coordinates.Y, float32 x.CurWeight / 10.0f))
    member this.EventTime = eventTime

type MyForm(numberCreature : int, sizeWorld : int) as this =
    inherit Form(   
                    Text = "My World",
                    MaximizeBox = false,
                    MinimizeBox = false,
                    FormBorderStyle = FormBorderStyle.Fixed3D,
                    Height = sizeWorld + 40,
                    Width = sizeWorld + 20,
                    BackColor = Color.WhiteSmoke
                )
    
    let world = new World(numberCreature, sizeWorld)
 
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
                            // Отрисовка всех существ виде кругов
                            List.iter (fun (X, Y, R) -> 
                                g.FillEllipse(green, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingGrass
                            List.iter (fun (X, Y, R) -> 
                                g.FillEllipse(blue, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingSheep
                            List.iter (fun (X, Y, R) ->
                                g.FillEllipse(red, X - R, Y - R, 2.0f * R, 2.0f * R)) world.ListDrawingWolf
                      )    

Application.Run(new MyForm(10, 500))