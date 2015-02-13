import Geometry (arc)

import Color as C
import Graphics.Collage as GC
import Graphics.Collage (defaultLine)
import Mouse
import Signal ((<~))

cX = 1000
cY = 500

main = plotArc 50 (0, 0) << canvasCoords <~ Mouse.position

canvasCoords : (Int, Int) -> (Float, Float)
canvasCoords (x, y) = (toFloat x - cX/2, cY/2 - toFloat y)

plotArc h tail head =
  GC.collage cX cY [ arc h tail head |> GC.path |> GC.traced { defaultLine | cap <- GC.Round, width <- 5 } ]
