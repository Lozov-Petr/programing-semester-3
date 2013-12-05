namespace MainSystem

    type private SubsystemA() =
     
        member x.Action1(n : int, f : float) =
            printfn " Выполнено действие 1 подсистемы A с аргументами:"
            printfn "   n = %A" n
            printfn "   f = %A\n" f

        member x.Action2(s : string, b : bool) =
            printfn " Выполнено действие 2 подсистемы A с аргументами:"
            printfn "   s = %A" s
            printfn "   b = %A\n" b

    type private SubsystemB() =
     
        member x.Action1(c : char, opt : Option<int>) =
            printfn " Выполнено действие 1 подсистемы B с аргументами:"
            printfn "   n = %A" c
            printfn "   f = %A\n" opt

        member x.Action2(list : List<int>, arr : float[]) =
            printfn " Выполнено действие 2 подсистемы B с аргументами:"
            printfn "   list = %A" list
            printfn "   arr = %A\n" arr

    type Facade() =
        
        static let A = new SubsystemA()
        static let B = new SubsystemB()

        static member Operation1(n : int, c : char) =
            printfn "Выполняется операция 1...\n"
            A.Action1(n, 3.14)
            B.Action1(c, None)
            printfn "Операция 1 выполнена.\n"
            printfn "-------------------------------------------------\n"
    
        static member Operation2(list : List<int>) =
            printfn "Выполняется операция 2...\n"
            B.Action1('e', Some list.Head)
            B.Action2(list, [|1.4; 1.7|])
            printfn "Операция 2 выполнена.\n"
            printfn "-------------------------------------------------\n"

        static member Operation3() =
            printfn "Выполняется операция 3...\n"
            A.Action1(0, 0.0)
            printfn "Операция 3 выполнена.\n"
            printfn "-------------------------------------------------\n"

namespace Program

    open MainSystem

    module Main =
        
        [<EntryPoint>]
        let main argv =
            Facade.Operation2([1;2;3;0])
            Facade.Operation1(3, 'n')
            Facade.Operation3()
            0
