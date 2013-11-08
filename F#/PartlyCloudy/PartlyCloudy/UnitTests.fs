module UnitTests

open Interfaces
open ATD
open FsUnit
open NUnit.Framework
open Program

type testWeather(daylight : DaylightType, speed : int, isShining : bool) =
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
    member x.``Если солнечно/лунно и нет ветра, то родится щенок`` () =
        let cloud = new Cloud(testWeather(Night, 0, true)) 
        cloud.Create |> should equal CreatureType.Puppy
