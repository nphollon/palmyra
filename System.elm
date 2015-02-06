module System where

import List as L

type alias System = {
    ply:Int,
    stocks:List (String, Int),
    flows:List (String, String)
  }

new : List (String, Int) -> List (String, String) -> System
new s f = { ply=0, stocks=s, flows=f }

update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve system = { system | ply <- 1 + system.ply
                         , stocks <- L.map bump system.stocks }

bump (name, size) = (name, size + 1)