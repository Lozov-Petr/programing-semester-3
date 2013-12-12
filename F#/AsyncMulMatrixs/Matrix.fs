module Matrix

type MyMatrix(arr : int[,]) =
    
    let height = Array2D.length1 arr
    let width  = Array2D.length2 arr

    member x.Height = height
    member x.Width = width
    member x.Get a b = arr.[a,b]
    static member Create a = new MyMatrix(a)

    override x.ToString() = sprintf "%A" arr

    static member Mul (a : MyMatrix) (b : MyMatrix) (countWorkflows : int) = 
        if a.Width <> b.Height then failwith "Ширина первой матрицы неравна высоте второй матрицы."

        let modulo  = a.Width % countWorkflows
        let floor   = a.Width / countWorkflows
        let ceiling = floor + (if modulo <> 0 then 1 else 0)

        let arrBorder = [|0..countWorkflows|]
                        |> Array.map (fun i -> if i < modulo then i * ceiling else i * floor + modulo)
        
        let getAsyncPart i = 
            let calcElem x y = List.map (fun n -> a.Get x n * b.Get n y) [0..a.Width - 1] |> List.sum                          
            let x = arrBorder.[i]
            let y = arrBorder.[i + 1] - 1
            async {return Array2D.initBased x 0 (y - x + 1) b.Width calcElem}
        
        let listArrToArr (list : int[,][]) = 
            let getIndex n = if n < modulo * ceiling then n / ceiling else (n - modulo) / floor
            let getOfList x y = list.[getIndex x].[x,y]  
            Array2D.init a.Height b.Width getOfList

        [0..countWorkflows - 1]
        |> List.map getAsyncPart
        |> Async.Parallel
        |> Async.RunSynchronously
        |> listArrToArr
        |> MyMatrix.Create
   