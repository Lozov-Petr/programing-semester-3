module TestMatrix

open System
open System.Diagnostics
open Matrix

type Test() =

    static let rnd = new Random(0)
    static let time = new Stopwatch()
    static let arrWorkflows = [1..8]

    static member private CreateRndMatrix size =
        (fun _ _ -> rnd.Next 9)
        |> Array2D.init size size
        |> MyMatrix.Create

    static member private CreateListRndMatrix() =
        List.init 10 (Test.CreateRndMatrix << ((*) 100) << ((+) 1))

    static member private CreateListTwoRndMatrix() =
        List.zip (Test.CreateListRndMatrix()) (Test.CreateListRndMatrix())

    static member private TestForOneWorkflow a b n =
        time.Restart()         
        ignore <| MyMatrix.Mul a b n
        time.Stop()
        let endOfWord = if n = 1 then "е " else "ах"
        printfn "   При %d поток%s время работы составило: %A" n endOfWord time.Elapsed

    static member private TestForAllWorkflows (a : MyMatrix, b : MyMatrix) =
        printfn "\nРазмер матрицы: %d" a.Height
        List.iter (Test.TestForOneWorkflow a b) arrWorkflows

    static member Run() =
        List.iter Test.TestForAllWorkflows <| Test.CreateListTwoRndMatrix()
