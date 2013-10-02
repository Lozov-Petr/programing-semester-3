type markov_automatic =
  
    val private list_sample : (string * string) list

    new (list : (string * string) list) = {list_sample = list}

    member this.run(str : string) =

        let rec search_sample (str : string) list =
            match list with
            | (str1 : string, str2) :: tl ->
                let num_sym = str.IndexOf(str1)
                if num_sym <> -1 then 
                                     let new_str = (str.[0 .. num_sym - 1] + str2 + str.[num_sym + str1.Length .. str.Length - 1])
                                     printfn "%s" new_str
                                     search_sample new_str (this.list_sample)
                                 else search_sample str tl
            | _ -> str
        
        printfn "%s" str 
        search_sample str this.list_sample


let list11 = [for i in 0..8 -> (string i + "#", string (i + 1))] @ [("9#", "#0"); ("#", "1")]
let list12 = [for i in 0..9 -> (string i + "*", "*" + string i)]
let list13 = [for i in 1..9 -> (string i + "!", "*" + string (i - 1))] @ [("0!", "!9")]
let list14 = [("0=", "!9=")] @ [for i in 1..9 -> (string i + "=", "*" + string (i - 1) + "=")]
let list1 = [("+*", "#+")] @ list11 @ [("+0", "+")] @ list12 @ list13 @ list14 @ [("+=?", "")]

let lNum = [for i in 0..9 -> string i] @ ["="; "+"]
let lChar = [for i in 'a'..'j' -> string i] @ ["$"; "|"]

let list21 = [for i in 0..11 -> (lNum.[i] + "?", "?" + lNum.[i] + lChar.[i])] @ [("?", "")]
let list22 =  List.fold (fun acc i -> acc @ [for j in lNum -> (i + j, j + i)]) [] lChar
let list23 = [for i in 0..10 -> lChar.[i] + "=", "=" + lChar.[i]]

let list24 = ("|*", "#|")::[for i in 0..8 -> (lChar.[i] + "#", lChar.[i + 1])] @ [("j#", "#a"); ("#", "b")]
let list25 = ("|a", "|")::[for i in 0..9 -> (lChar.[i] + "*", "*" + lChar.[i])]
let list26 = [for i in 1..9 -> (lChar.[i] + "!", "*" + lChar.[i - 1])] @ [("a!", "!j")]
let list27 = [("a$", "!j$")] @ [for i in 1..9 -> (lChar.[i] + "$", "*" + lChar.[i - 1] + "$")]
let list28 = ("|$", "%")::[for i in 0..9 -> (lChar.[i] + "%", "%" + lNum.[i])] 

let list2 = list21 @ list22 @ list23 @ list24 @ list25 @ list26 @ list27 @ list28 @ [("%", "")]


let mark1 = new markov_automatic(list1)
let mark2 = new markov_automatic(list2)
let res1 = mark1.run("1+99=?")
let res2 = mark2.run("1+99=?")
