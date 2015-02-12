import Arc (arc)

import Color as C
import Graphics.Collage as GC
import Mouse
import Signal ((<~))

cX = 1000
cY = 500

main = plotArc 100 (0, 0) << canvasCoords <~ Mouse.position

canvasCoords : (Int, Int) -> (Float, Float)
canvasCoords (x, y) = (toFloat x - cX/2, cY/2 - toFloat y)

plotArc h head tail =
  GC.collage cX cY [ 
  GC.circle 5 |> GC.filled C.red |> GC.move head,
  GC.circle 5 |> GC.filled C.blue |> GC.move tail,
  arc 30 h tail head |> GC.traced GC.defaultLine  ]
