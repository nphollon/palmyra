module System where

import System.Stock as SS
import System.Flow as SF

import Dict as D
import List as L

type alias System = {
    ply:Int,
    stocks:SS.StockRepo,
    flows:List SF.Flow
  }

new : List SS.Stock -> List (Int, String, String) -> System
new s f = 
  let
    stocks = SS.repository s
    flows = L.map (\(r, i, o) -> SF.Pipe [] r i o) f
  in { ply=0, stocks=stocks, flows=flows }

getInfo : System -> (D.Dict String String, List String)
getInfo sys = (SS.stocksInfo sys.stocks, SF.flowsInfo sys.flows)

update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlow, newStocks) = L.foldr SF.addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlow }
