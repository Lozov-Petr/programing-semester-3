module Program

open ATD
open Interfaces

type Creature(creatureType : CreatureType) =
    member x.CreatureType = creatureType

type Magic() =
    interface IMagic with
        member x.CallCourier _ = ()

type Cloud(weather : IWeatherFactory) =
    let daylight = weather.createDaylight
    let luminary = weather.createLuminary
    let wind     = weather.createWind  
 
    member private x.InternalCreate() =
        let creatureType =
            match (daylight.Current, luminary.IsShining(), wind.Speed) with
            | (_, true, 0) -> Puppy
            | (_, false, 10) -> Bat
            | (Morning, true, speed) when speed >= 1 && speed <= 4 -> Kitten
            | (Day, false, speed) when speed >= 5 && speed <= 8 -> Piglet
            | (Evening, true, speed) when speed >= 2 && speed <= 5 -> Bearcub
            | (Night, _, speed) when speed >= 3 && speed <= 4 -> Hedgehog
            | _ -> Balloon
        new Creature(creatureType)

    member x.Create() =
      let creature = x.InternalCreate() 
      let magic = new Magic() :> IMagic
      magic.CallCourier(creature.CreatureType)
      creature