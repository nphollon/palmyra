import Geometry as Geo

import Color as C
import Graphics.Collage as GC
import Graphics.Collage (defaultLine)
import Mouse
import Signal ((<~))
import List as L

cX = 1000
cY = 500

main = plotArc 50 (0, 0) << canvasCoords <~ Mouse.position

canvasCoords : (Int, Int) -> (Float, Float)
canvasCoords (x, y) = (toFloat x - cX/2, cY/2 - toFloat y)

plotArc h tail head =
  let
    arc = Geo.arc h tail head
    arrowhead = Geo.arrowhead 10 (degrees 45) (L.head arc) (L.head <| L.tail arc)
  in GC.collage cX cY [ 
    arc |> GC.path |> GC.traced defaultLine,
    arrowhead |> GC.polygon |> GC.filled C.black
    ]
