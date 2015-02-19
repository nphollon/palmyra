module System where

import System.Stock as SS
import System.Flow as SF

import Array as A
import Dict as D
import List as L

type alias System = {
    ply:Int,
    stocks:SS.StockRepo,
    flows:List SF.Flow
  }

type alias SystemParams = {
  stocks:SS.StockRepo,
  flows:List (SF.StockLink {})
}

type alias Id = Int

new : SystemParams -> System
new sys = { ply = 0
          , stocks = sys.stocks
          , flows = L.map SF.new sys.flows
        }

getInfo : System -> (List (Id, String), List (Id, Id))
getInfo sys = (SS.stocksInfo sys.stocks, SF.flowsInfo sys.flows)

update : Id -> System -> System
update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlow, newStocks) = L.foldr SF.addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlow }
