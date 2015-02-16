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

type alias Id = Int
type alias Rate = Float

new : List (Id, SS.Stock) -> List (Rate, Id, Id) -> System
new s f = 
  let
    stocks = D.fromList s
    flows = SF.initAll stocks f
  in { ply=0, stocks=stocks, flows=flows }

getInfo : System -> (List (Int, String), List (Int, Int))
getInfo sys = (SS.stocksInfo sys.stocks, SF.flowsInfo sys.flows)

update : Int -> System -> System
update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlow, newStocks) = L.foldr SF.addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlow }
