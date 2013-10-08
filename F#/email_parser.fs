open System.Text.RegularExpressions

let is_properly_email str =
    let str_domens = "((aero)|(asia)|(coop)|(info)|(jobs)|(mobi)|(museum)|(name)|(travel)|([a-z]{2,3}))"
    let sample = "([a-zA-Z_0-9]+\.?)+[a-zA-Z0-9_-]+@([a-z]+\.)+" + str_domens
    ((new Regex("^" + sample  + "$")).Match str).Success

// right examples
printfn "%A" (is_properly_email "LoZoV.PeTeR@spb.spbu.matmex.ru" = true)
printfn "%A" (is_properly_email "not_kill@mail.ru" = true)
printfn "%A" (is_properly_email "n.o.t._.k.i.l.l@mail.ru" = true)
printfn "%A" (is_properly_email "not-kill@mail.ru" = true)
printfn "%A" (is_properly_email "not.kill@mail.ru" = true)
printfn "%A" (is_properly_email "not_kill@mail.spbu.com" = true)
// wrong examples
printfn "%A" (is_properly_email "not@kill@mail.ru" = false)
printfn "%A" (is_properly_email "not kill@mail.ru" = false)
printfn "%A" (is_properly_email "not_kill@mail..ru" = false)
printfn "%A" (is_properly_email "not_kill@mail.ruru" = false)
printfn "%A" (is_properly_email "not_kill.@mail.ru" = false)
printfn "%A" (is_properly_email "not_kill.mail.ru" = false)
printfn "%A" (is_properly_email "not_kill@.mail.ru" = false)
printfn "%A" (is_properly_email "not_kill@mail.r" = false)