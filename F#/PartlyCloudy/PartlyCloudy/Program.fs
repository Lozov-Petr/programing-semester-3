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
 
    member x.InternalCreate() =
      if daylight.Current = DaylightType.Night then
        if luminary.IsShining() then
          new Creature(CreatureType.Bat)
        else
          raise <| new System.NotImplementedException()
      else
        raise <| new System.NotImplementedException()
 
    member x.Create() =
      let creature = x.InternalCreate()
      let magic = new Magic()    
 //     magic.CallCourier(creature.CreatureType)
      creature