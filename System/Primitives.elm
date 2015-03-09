module System.Primitives where

import Dict as D
import List as L
import String

type alias Id = String
type alias Viewable a = { a | view : Float }

type DataBank a = Viewable {
    deposit : Float -> DataBank,
    withdraw : Float -> (Float, DataBank)
  })

type DataMover
  = Unary (Viewable { target : Id, adjust : DataBank -> DataBank })
  | Binary (Viewable { target : (Id,Id), adjust : (DataBank,DataBank) -> (DataBank,DataBank) })

type Component = Container DataBank | Connector DataMover
type System = World (D.Dict Id Component)

evolve : System -> System
evolve sys = L.foldl moveData sys (connectors sys)

moveData : DataMover -> System -> System
moveData mover =
  case mover of
    Unary x -> unaryMove x
    Binary x -> binaryMove x

unaryMove mover system =
  let mayb = getBank mover.target system
  in case mayb of
    Nothing -> system
    Just b -> updateBank mover.target (mover.adjust b) system

binaryMove mover system =
  let
    mayb = getBanks mover.target system
  in case mayb of
    Nothing -> system
    Just (a,b) -> updateBanks mover.target (mover.adjust (a,b)) system

getBank : Id -> System -> Maybe DataBank
getBank id (World dict) =
  case (D.get id dict) of
    Just (Container x) -> Just x
    otherwise -> Nothing

getBanks : (Id, Id) -> System -> Maybe (DataBank, DataBank)
getBanks (i,j) (World dict) =
  let values = (D.get i dict, D.get j dict)
  in case values of
    (Just (Container a), Just (Container b)) -> Just (a,b)
    otherwise -> Nothing

updateBank : Id -> DataBank -> System -> System
updateBank k v (World dict) = World (D.intersect (D.singleton k (Container v)) dict)

updateBanks : (Id,Id) -> (DataBank,DataBank) -> System -> System
updateBanks (i,j) (x,y) = updateBank i x >> updateBank j y

connectors : System -> List DataMover
connectors (World pieces) =
  let getDataMover piece =
    case piece of
      Connector x -> Just x
      otherwise -> Nothing
  in L.filterMap getDataMover (D.values pieces)

type alias Info = (List (Id, String), List ((Id, Id), List Float))

getInfo : System -> Info
getInfo (World dict) = L.foldl sortComponents ([],[]) (D.toList dict)

sortComponents : (Id, Component) -> Info -> Info
sortComponents (i, c) (stocks, flows) =
  let
    name data = i ++ " " ++ (format data)
    addStock data = ((i,name data)::stocks,flows)
    addFlow target = (stocks,(target,[1])::flows)
  in case c of
  Container (Bank a) -> addStock a.view
  Connector (Binary a) -> addFlow a.target
  otherwise -> (stocks, flows)

format : Float -> String
format x =
  let
    totalCents = round (x * 100)
    dollars = totalCents // 100 |> toString
    cents = totalCents `rem` 100 |> abs |> toString |> String.pad 2 '0'
  in dollars ++ "." ++ cents











