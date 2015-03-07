module System.Primitives where

import Dict as D
import List as L

type alias Id = String
type alias Viewable a = { a | view : Float }

type DataBank = Bank Viewable {
    deposit : Float -> DataBank,
    withdraw : Float -> (Float, DataBank)
  }

type DataMover
  = Unary Viewable { target : Id, adjust : DataBank -> DataBank }
  | Binary Viewable { target : (Id,Id), adjust : (DataBank,DataBank) -> (DataBank,DataBank) }

type Component = Container DataBank | Connector DataMover
type System = World (D.Dict Id Component)

evolve : System -> System
evolve sys = foldl moveData sys (connectors sys)

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
updateBank k v (World dict) = World (D.intersect (D.singleton k v) dict)

updateBanks : (Id,Id) -> (DataBank,DataBank) -> System -> System
updateBanks (i,j) (x,y) = updateBank i x >> updateBank j y

connectors : System -> List DataMover
connectors World pieces =
  let getDataMover piece =
    case piece of
      Connector x -> Just x
      otherwise -> Nothing
  in L.filterMap getDataMover (D.values pieces)