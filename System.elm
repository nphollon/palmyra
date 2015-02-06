module System where

import List as L

type alias System = {
    time:Float,
    stocks:List (String, Int),
    flows:List (String, String)
  }

new : List (String, Int) -> List (String, String) -> System
new s f = { time=0, stocks=s, flows=f }

evolve : Float -> System -> System
evolve dt system = { system | time <- dt + system.time }
