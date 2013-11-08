module UnitTests

open Interfaces
open ATD
open FsUnit
open NUnit.Framework
open Program

type testWeather(daylight : DaylightType, isShining : bool, speed : int) =
    interface IWeatherFactory with
        member x.createDaylight = 
                {new IDaylight with
                    member y.Current = daylight
                }

        member x.createWind = 
                {new IWind with
                    member y.Speed = speed
                }

        member x.createLuminary = 
                {new ILuminary with
                    member y.IsShining() = isShining
                }

[<TestFixture>]
type Testing() =

    [<Test>]
    member x.``Щенок родился при ясном небе и отсутствии ветра (0)`` () =
        [Morning; Day; Evening; Night] 
        |> List.map (fun x -> new Cloud(testWeather(x,true,0))) 
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Puppy)

    [<Test>]
    member x.``Летучая мышь родилась при пасмурной погоде и очень сильном ветре (10)`` () =
        [Morning; Day; Evening; Night] 
        |> List.map (fun x -> new Cloud(testWeather(x,false,10)))
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Bat)
    
    [<Test>]
    member x.``Котёнок родился солнечным утром при слабом ветре (1-4)`` () =
        [1..4]
        |> List.map (fun x -> new Cloud(testWeather(Morning,true,x)))
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Kitten)
     
    [<Test>]
    member x.``Поросёнок родился пасмурным днём при сильном ветре (5-8)`` () =
        [5..8]
        |> List.map (fun x -> new Cloud(testWeather(Day,false,x)))
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Piglet)

    [<Test>]
    member x.``Медвежонок родился сонечным вечером при умеренном ветре (2-5)`` () =
        [2..5]
        |> List.map (fun x -> new Cloud(testWeather(Evening,true,x)))
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Bearcub)

    [<Test>]
    member x.``Ёж родился ночью при слабом ветре (3-4)`` () =
        [3;4]
        |> List.map (fun x -> List.map (fun y -> new Cloud(testWeather(Night,y,x))) [false;true])
        |> List.concat
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Hedgehog)

    [<Test>]
    member x.``Воздушный шарик родился, если не родилось животное (проверено несколько случаев)`` () =
        [(Morning,true,9);(Day,false,2);(Evening,true,6);(Night,false,6)]
        |> List.map (fun x -> new Cloud(testWeather x))
        |> List.iter (fun x -> x.Create().CreatureType |> should equal Balloon)
