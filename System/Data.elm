module System.Data where

import System.Stock (Stock(..), Id, StockRepo)
import System.Flow (Flow)

import System.Stock as SS
import System.Flow as SF

import Dict as D
import List as L

type Component = Node Id String | Arc Id Id String
repoNodes : StockRepo -> List Component
repoNodes ss = D.toList ss |> L.map (\(i,s) -> stockNode i s)

stockNode : Id -> Stock -> Component
stockNode i s = Node i (SS.stockInfo s)

isNode : Component -> Bool
isNode c =
  case c of
    Node _ _ -> True
    otherwise -> False

id : Component -> Id
id c =
  case c of
    Node i _ -> i
    otherwise -> -1

label : Component -> String
label c =
  case c of
    Node _ s -> s
    Arc _ _ s -> s