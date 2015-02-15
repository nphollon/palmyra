module Thermostat (thermostat) where

opacity = Ply 1000
outsideTemp = As 265
targetTemp = As 290
triggerTemp = As 287
heat = AsPerPly 0.1

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
    flux _ _ = AsPerPly 0
    tooCold _ roomTemp = roomTemp < triggerTemp
  in { flux = flux, rules = [(tooCold, 0)] }

coolRoom = { source=2, sink=3, state=0, states=coolStates }
coolStates = Dict.singleton 0 cooling
cooling =
  let flux roomTemp _ = (roomTemp - outsideTemp) / opacity
  in { flux = flux, rules = [] }
