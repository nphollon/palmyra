module System where

import Array as A
import Dict as D
import List as L
import Maybe as M
import Signal as S
import String as Str

type alias Id = String

type System = Systemic {
    stocks : D.Dict Id Float,
    flows : D.Dict Id Flow,
    rules : D.Dict Id Rule
  }

type alias Rule = {
    target : Id,
    rule : System -> Maybe Float
  }

type Flow = Growth Id Float | Decay Id Float Float | Constant Id Float

getInfo : System -> (List (Id, String), List (Id, Id))
getInfo (Systemic sys) = (stocksInfo sys.stocks, flowsInfo sys.flows)

evolve : System -> System
evolve (Systemic sys) =
  let newFlowSys = L.foldl applyRule (Systemic sys) (D.values sys.rules)
  in L.foldl applyFlow newFlowSys (D.values sys.flows)

applyRule : Rule -> System -> System
applyRule r (Systemic sys) =
  let
    k = r.target
    v = r.rule (Systemic sys)
    newFlows = D.update k (updateRate v) sys.flows
  in (Systemic { sys | flows <- newFlows })

updateRate : Maybe Float -> Maybe Flow -> Maybe Flow
updateRate mv mf = case (mv, mf) of
  (Just v, Just f) -> Just (setRate v f)
  otherwise -> mf

applyFlow : Flow -> System -> System
applyFlow f (Systemic sys) = Systemic { sys | stocks <- sourceToSink f sys.stocks }


view : Id -> System -> Maybe Float
view i (Systemic sys) =
  let
    stockView = D.get i sys.stocks
    flowView = D.get i sys.flows |> M.map getRate
  in M.oneOf [ stockView, flowView ]

sourceToSink : Flow -> D.Dict Id Float -> D.Dict Id Float
sourceToSink flow =
  let update id = D.update id << M.map
  in case flow of
    Growth id r -> update id <| \v -> deposit (r*v) v
    Decay id r v0 -> update id <| \v -> deposit (r * (v0 - v)) v
    Constant id r -> update id <| deposit r




stocksInfo : D.Dict Id Float -> List (Id, String)
stocksInfo = D.map (\k v -> k ++ " : " ++ format v) >> D.toList

format : Float -> String
format x =
  let
    totalCents = round (x * 100)
    dollars = totalCents // 100 |> toString
    cents = totalCents `rem` 100 |> abs |> toString |> Str.pad 2 '0'
  in dollars ++ "." ++ cents

deposit : Float -> Float -> Float
deposit dx x = max 0 (dx + x)

withdrawById : Float -> Id -> D.Dict Id Float -> (Float, D.Dict Id Float)
withdrawById dx id ss =
  case (D.get id ss) of
    Nothing -> (0, ss)
    Just x ->
      let (dx', x') = withdraw dx x
      in (dx', D.insert id x' ss)

withdraw : Float -> Float -> (Float, Float)
withdraw dx x =
  if x > dx 
  then (dx, x - dx)
  else (x, 0)


flowsInfo : D.Dict Id Flow -> List (Id, Id)
flowsInfo = D.values >> L.map print

print : Flow -> (Id, Id)
print flow = ("","")

setRate : Float -> Flow -> Flow
setRate r flow =
  case flow of
    Growth a _ -> Growth a r
    Decay a _ b -> Decay a r b
    Constant a _ -> Constant a r

getRate : Flow -> Float
getRate flow =
  case flow of
    Growth _ r -> r
    Decay _ r _ -> r
    Constant _ r -> r



transform2 : Id -> Id -> (Float -> Float -> Float) -> System -> Maybe Float
transform2 i j f sys =
  case (view i sys, view j sys) of
    (Just x, Just y) -> Just (f x y)
    otherwise -> Nothing