module Interface.System (draw) where

import Interface.Geometry as Geo

import Array as A
import Color as C
import Dict as D
import Graphics.Collage as GC
import Graphics.Element (Element)
import List as L
import Text as T

type alias Point = (Float, Float)

draw : List (comparable, String) -> List (comparable, comparable) -> Element
draw stocks flows =
  let
    pos = A.fromList stocks |> addPositions
    stockForms = L.map drawStock (D.values pos)
    flowForms = L.map (drawFlow pos) flows
  in GC.collage 500 300 (flowForms ++ stockForms)

drawStock : (String, (Point, Point)) -> GC.Form
drawStock (label, (nodePosition, infoboxPosition)) =
  let    
    info = T.plainText label |> GC.toForm
    box = GC.filled C.lightBlue (GC.rect 100 50)
    infobox = GC.group [ box, info ] |> GC.move infoboxPosition

    node = GC.filled C.red (GC.circle 6) |> GC.move nodePosition
    link = GC.traced (GC.dotted C.black) (GC.segment nodePosition infoboxPosition)
  in GC.group [ link, node , infobox ]

drawFlow : D.Dict comparable (a, (Point, Point)) -> (comparable, comparable) -> GC.Form
drawFlow pos flow =
  let
    tail = D.get (fst flow) pos
    head = D.get (snd flow) pos
  in case (tail, head) of
    (Just (_,(t,_)), Just (_,(h,_))) -> arc t h
    otherwise -> blank

arc : Point -> Point -> GC.Form
arc t h = 
  let 
    margin = 15
    isWithinBounds p = not (Geo.isWithin margin t p) && not (Geo.isWithin margin h p)
    arc = Geo.arc 5 h t |> L.filter isWithinBounds
    arrowhead = Geo.arrowhead 10 (degrees 45) (L.head arc) (L.head <| L.tail arc)
  in GC.group [
      arc |> GC.path |> GC.traced (GC.solid C.black),
      arrowhead |> GC.polygon |> GC.filled C.black
    ]

addPositions : A.Array (comparable, b) -> D.Dict comparable (b, (Point, Point))
addPositions kvPairs =
  let
    n = A.length kvPairs
    position i (k, v) = (k, (v, keyPositions n i))
  in A.indexedMap position kvPairs |> A.toList |> D.fromList

keyPositions : Int -> Int -> (Point, Point)
keyPositions n i =
  let
    n' = toFloat n
    i' = toFloat i
    angle = (i' + 0.5) / n' * degrees 360
    nodePosition = polar 100 angle

    (row, col) = if 2 * i + 1 > n then (n' - i', 1) else (i' + 1, -1)
    rowOffset = 0.5 * toFloat (1 + ceiling (0.5 * n'))
    infoboxPosition = (col * 180, (rowOffset - row) * 70)
  in (nodePosition, infoboxPosition)

polar : Float -> Float -> Point
polar r theta = Geo.polar (0,0) r (theta + degrees 90)

blank : GC.Form
blank = GC.group []