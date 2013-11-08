module Interfaces

open ATD

type IMagic =
    abstract member CallCourier : CreatureType -> unit

type IDaylight =
    abstract member Current : DaylightType

type IWind =
    abstract member Speed : int

type ILuminary =
    abstract member IsShining : unit -> bool

type IWeatherFactory =
    abstract member createDaylight : IDaylight
    abstract member createWind     : IWind
    abstract member createLuminary : ILuminary