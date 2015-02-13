import Geometry as Geo

import Color as C
import Dict as D
import Graphics.Collage as GC
import List as L
import Text as T

type alias Point = (Float, Float)

stocks = D.fromList [(1, "Eins"), (2, "Zwei"), (3, "Drei"), (4, "Vier"), (5, "Fünf"), (6, "Sechs"), (7, "Sieben")]
flows = [
  (1,2), (1,6),
  (2,4), (2,5),
  (3,5), (3,7),
  (4,3), (4,7),
  (5,1), (5,3),
  (6,1), (6,4),
  (7,2), (7,6)
  ]

main = GC.collage 500 500 drawAllStocks

drawAllStocks =
  let
    pos = graphIds stocks
    label = D.values stocks
    stockForms = L.map2 drawStock (D.values pos) label
    flowForms = L.map (drawFlow pos) flows
  in flowForms ++ stockForms

drawStock : (Point, Point) -> String -> GC.Form
drawStock (nodePosition, infoboxPosition) label =
  let    
    info = T.plainText label |> GC.toForm
    box = GC.filled C.lightBlue (GC.rect 100 50)

    node = GC.filled C.red (GC.circle 6) |> GC.move nodePosition
    infobox = GC.group [ box, info ] |> GC.move infoboxPosition
    link = GC.traced (GC.dotted C.black) <| GC.segment nodePosition infoboxPosition
  in GC.group [ link, node , infobox ]

drawFlow : D.Dict Int (Point, Point) -> (Int, Int) -> GC.Form
drawFlow pos flow =
  let
    tail = D.get (fst flow) pos
    head = D.get (snd flow) pos
  in case (tail, head) of
    (Just (t,_), Just (h,_)) -> arc t h
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

graphIds : D.Dict Int a -> D.Dict Int (Point, Point)
graphIds dict =
  let
    keys = D.keys dict
    n = L.length keys
  in L.map2 (\k i -> (k, keyPositions n i)) keys [ 1 .. n ] |> D.fromList

keyPositions : Int -> Int -> (Point, Point)
keyPositions n i =
  let
    n' = toFloat n
    i' = toFloat i
    angle = (i' - 0.5) / n' * degrees 360
    nodePosition = polar 70 angle

    column = if i * 2 > n then -1 else 1
    row = if i * 2 > n then n' + 1 - i' else i'
    rowOffset = 0.5 * toFloat (1 + ceiling (0.5 * n'))
    infoboxPosition = (column * 180, (rowOffset - row) * 70)
  in (nodePosition, infoboxPosition)

polar : Float -> Float -> Point
polar r theta = 
  let
    x = r * (sin theta)
    y = r * (cos theta)
  in (x,y)
