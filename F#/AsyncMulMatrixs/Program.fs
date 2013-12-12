module Program

open TestMatrix
   
[<EntryPoint>]
let main argv =
    Test.Run()
    0