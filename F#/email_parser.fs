(******************************
           Лозов Пётр
           Группа 271
            08.11.13
        Парсер e-mail'ов
   [+ модульное тестирование]
 *******************************)

open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit

let isProperlyEmail str =
    let strDomens = "(aero|asia|coop|info|jobs|mobi|museum|name|travel|[a-z]{2,3})"
    let sample = "[a-zA-Z_][a-zA-Z0-9_-]*(\.?[a-zA-Z_0-9]+)*@([a-z]+\.)+" + strDomens
    ((new Regex("^" + sample  + "$")).Match str).Success


[<TestFixture>]
type MailTest() = 
    
    let oldRightTests = [
                            "LoZoV.PeTeR@spb.spbu.matmex.ru"
                            "not_kill@mail.ru"
                            "n.o.t._.k.i.l.l@mail.ru"
                            "not-kill@mail.ru"
                            "not.kill@mail.ru"
                            "not_kill@mail.spbu.com"
                        ]

    let oldWrongTests = [
                            "not@kill@mail.ru"
                            "not kill@mail.ru"
                            "not_kill@mail..ru" 
                            "not_kill@mail.ruru"
                            "not_kill.@mail.ru"
                            "not_kill.mail.ru"
                            "not_kill@.mail.ru"
                            "not_kill@mail.r"
                        ]

    [<Test>]
    member this.``Старые тесты`` 
        ()= oldRightTests |> List.iter (fun x -> isProperlyEmail x |> should be True)
            oldWrongTests |> List.iter (fun x -> isProperlyEmail x |> should be False)

    [<Test>]
    member this.``Очень маленький, но правильный e-mail`` 
        ()= isProperlyEmail "a@b.cc" |> should be True

    [<Test>]
    member this.``Просто обязан быть распознанным`` 
        ()= isProperlyEmail "victor.polozov@mail.ru" |> should be True

    [<Test>]
    member this.``Самый обычный e-mail`` 
        ()= isProperlyEmail "my@domain.info" |> should be True

    [<Test>]
    member this.``Первый символ '_', это норма`` 
        ()= isProperlyEmail "_.1@mail.com" |> should be True

    [<Test>]
    member this.``Большой и страшный, но правильный e-mail`` 
        ()= isProperlyEmail "coins_department@hermitage.museum" |> should be True

    [<Test>]
    member this.``Хоть маленький, а неправильный e-mail`` 
        ()= isProperlyEmail "a@b.c" |> should be False

    [<Test>]
    member this.``Две точки подряд в e-mail'е не проходят`` 
        ()= isProperlyEmail "a..b@mail.ru" |> should be False

    [<Test>]
    member this.``Начинаться с точки e-mail не может`` 
        ()= isProperlyEmail ".a@mail.ru" |> should be False
    
    [<Test>]
    member this.``Не допускается длинный домен верхнего уровня`` 
        ()= isProperlyEmail ".yo@domain.somedomain" |> should be False

    [<Test>]
    member this.``Нельзя начинать e-mail с цифр`` 
        ()= isProperlyEmail "1@mail.ru" |> should be False   