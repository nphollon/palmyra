import Geometry as Geo

import Array as A
import Color as C
import Dict as D
import Graphics.Collage as GC
import Graphics.Element as GE
import List as L
import Text as T

type alias Point = (Float, Float)

s = [(5, "Fünf"), (3, "Drei"), (4, "Vier"),  (6, "Sechs"), (7, "Sieben"), (2, "Zwei"), (1, "Eins")] |> A.fromList
f = [
  (1,2), (1,6),
  (2,4), (2,5),
  (3,5), (3,7),
  (4,3), (4,7),
  (5,1), (5,3),
  (6,1), (6,4),
  (7,2), (7,6)
  ]

main =
  let diagram stocks = drawSystem stocks f |> GC.collage 500 500
  in GE.flow GE.left [ diagram s, diagram (A.slice 0 -1 s) ]

drawSystem stocks flows =
  let
    pos = addPositions stocks
    stockForms = L.map drawStock (D.values pos)
    flowForms = L.map (drawFlow pos) flows
  in flowForms ++ stockForms

drawStock : (String, (Point, Point)) -> GC.Form
drawStock (label, (nodePosition, infoboxPosition)) =
  let    
    info = T.plainText label |> GC.toForm
    box = GC.filled C.lightBlue (GC.rect 100 50)

    node = GC.filled C.red (GC.circle 6) |> GC.move nodePosition
    infobox = GC.group [ box, info ] |> GC.move infoboxPosition
    link = GC.traced (GC.dotted C.black) <| GC.segment nodePosition infoboxPosition
  in GC.group [ link, node , infobox ]

drawFlow : D.Dict comparable (b, (Point, Point)) -> (comparable, comparable) -> GC.Form
drawFlow pos flow =
  let
    tail = D.get (fst flow) pos
    head = D.get (snd flow) pos
  in case (tail, head) of
    (Just (_,(t,_)), Just (_,(h,_))) -> arc t h
    otherwise -> GC.group []

arc : Point -> Point -> GC.Form
arc t h = 
  let 
    margin = 15
    isWithinBounds p = not (Geo.isWithin margin t p) && not (Geo.isWithin margin h p)
    arc = Geo.arc 5 t h |> L.filter isWithinBounds
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
