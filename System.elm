module System where

import List as L
import Dict as D
import Maybe as M

type alias Stocks = D.Dict String Int
type alias Flows = List (String, String)
type alias System = {
    ply:Int,
    stocks:Stocks,
    flows:List (String, String)
  }

new : List (String, Int) -> Flows -> System
new s f = { ply=0, stocks=D.fromList s, flows=f }

update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  { ply=ply + 1, stocks=trickle stocks flows, flows=flows }

drain : Maybe Int -> Maybe Int
drain = M.map ((+) -1)

fill : Maybe Int -> Maybe Int
fill = M.map ((+) 1)

apply : Stocks -> (String, String) -> Stocks
apply stocks (source, sink) =
  D.update source drain stocks |> D.update sink fill

trickle : Stocks -> Flows -> Stocks
trickle stocks flows =
  case flows of
    f::fs -> trickle (apply stocks f) fs
    [] -> stocks