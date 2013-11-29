

(* Хранитель *)
type Memento (health : int) =
    member x.GetState() = health


(* Хозяин *)
type Player() =    
    let mutable health = 100
    
    member x.GetHurt(hurt : int) =
        health <- health - hurt
        
    member x.GetCure(cure : int) =
        health <- health + cure

    member x.PrintPulse() =
        if   health > 70  then printfn "Green"
        elif health <= 40 then printfn "Red"
        else                   printfn "Yellow"

    member x.CreateMemento() = 
        new Memento(health)

    member x.SetMemento(memento : Memento) = 
        health <- memento.GetState()   


(* Опекун *)
type Game() =
    let mutable optMemento : Option<Memento> = None

    member x.SaveState(player : Player) =
        optMemento <- Some <| player.CreateMemento()
        printfn "Save state"

    member x.LoadState(player : Player) =
        if optMemento.IsNone then failwith "Memento is Nome"
        player.SetMemento(optMemento.Value)
        printfn "Load state"


[<EntryPoint>]
let main argv =
    let game   = new Game()
    let player = new Player()
    
    player.GetHurt(20)
    player.GetHurt(30)
    player.GetHurt(20)
    player.PrintPulse()

    game.SaveState(player)

    player.GetCure(30)
    player.PrintPulse()

    game.LoadState(player)

    player.PrintPulse()
    
    0