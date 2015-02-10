module Thermostat (thermostat) where

type System = { stocks: Dict Id Stock, flows: List Flow }

type Stock = { name: String, value: Scalar }

type Flow = { source: Id, sink: Id, state: Id, states: Dict Id State }
type State = { flux: Flux, rules: List (Trigger, Id) }

type alias Flux = Scalar -> Scalar -> Float
type alias Trigger = Scalar -> Scalar -> Bool

type Scalar = Ground | Mass Float
type alias Id = Int

opacity = 1000
outsideTemp = 265
targetTemp = 290
triggerTemp = 287
heat = 0.1

thermostat = {
    stocks = Dict.fromList [ (1, furnace), (2, room), (3, ground) ],
    flows = [ heatRoom, coolRoom ]
  }

furnace = { name="Furnace", value=Ground }
room = { name="Room", value=Mass 275 }
ground = { name="Outside", value=Ground }

heatRoom = { source=1, sink=2, state=0, states=heatStates }
heatStates = Dict.fromList [ (0, heatOn), (1, heatOff) ]

heatOn =
  let
    flux _ _ = heat
    warmEnough _ roomTemp = roomTemp > targetTemp
  in { flux = flux, rules = [(warmEnough, 1)] }

heatOff =
  let
    flux _ _ = 0
    tooCold _ roomTemp = roomTemp < triggerTemp
  in { flux = flux, rules = [(tooCold, 0)] }

coolRoom = { source=2, sink=3, state=0, states=coolStates }
coolStates = Dict.singleton 0 cooling
cooling =
  let flux roomTemp _ = (roomTemp - outsideTemp) / opacity
  in { flux = flux, rules = [] }
