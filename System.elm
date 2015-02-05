module System where

import List as L

type alias System = { time:Float, stocks:List Stock, flows:List Flow }
type alias Stock = { name:String, size:Int }
type alias Flow = { source:String, sink:String }


stock : String -> Int -> System
stock name size = { time=0, stocks=[{name=name, size=size}], flows=[] }

flowTo : String -> Int -> System -> System
flowTo name size sys =
  let
    lastStock = L.head sys.stocks
    newStock = { name=name, size=size }
    newFlow = { source=lastStock.name, sink=name }
  in { sys | stocks <- newStock :: sys.stocks
           , flows <- newFlow :: sys.flows }

evolve : Float -> System -> System
evolve dt system = { system | time <- dt + system.time }
