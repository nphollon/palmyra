module System where

import System.Stock as SS
import System.Flow as SF
import System.Data as SD

import Dict as D
import List as L

type alias System = {
    ply:Int,
    stocks:SS.StockRepo,
    flows:List SF.Flow
  }

new : List SS.Stock -> List SF.FlowData -> System
new s f = 
  let
    stocks = SS.repository s
    flows = SF.initAll stocks f
  in { ply=0, stocks=stocks, flows=flows }

getInfo sys = SD.systemComponents sys.stocks sys.flows

update : Int -> System -> System
update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlow, newStocks) = L.foldr SF.addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlow }
