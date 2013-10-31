type MyEvent() =
    let e = new Event<int>()
    let mutable value = 0

    member x.GetValue = value

    member x.Raise v =
        value <- v
        e.Trigger v

    member x.Subscribe f = 
        e.Publish.Add(f)

    static member (+) (a : MyEvent, b : MyEvent) =
        let e = new MyEvent()
        a.Subscribe(fun _ -> e.Raise(a.GetValue + b.GetValue))
        b.Subscribe(fun _ -> e.Raise(a.GetValue + b.GetValue))
        e


let a = new MyEvent()
let b = new MyEvent()
let c = a + b 

a.Raise(1)
b.Raise(2)
printfn "%A" c.GetValue

b.Raise(4)
printfn "%A" c.GetValue
