module Arc where

import Color as C
import Graphics.Collage as GC
import List as L
import Mouse
import Signal ((<~))

cX = 1000
cY = 500

main = plotArc 5 (0, 0) << canvasCoords <~ Mouse.position

canvasCoords : (Int, Int) -> (Float, Float)
canvasCoords (x, y) = (toFloat x - cX/2, cY/2 - toFloat y)

plotArc h head tail =
  GC.collage cX cY [ 
  GC.circle 5 |> GC.filled C.red |> GC.move head,
  GC.circle 5 |> GC.filled C.blue |> GC.move tail,
  arc 100 tail head h |> GC.traced GC.defaultLine  ]

arc res tail head h =
  let
    mid = midpoint head tail
    l = length tail head
    r = (l^2 + 4 * h^2) / (8 * h)
    inclination = rotationAngle tail head + degrees 90
    arcCenter = polar mid (r-h) inclination

    arcLength = 2 * atan2 (0.5 * l) (r - h)
    tailAngle = rotationAngle arcCenter tail
    incrAngle i = arcLength * i * 0.01 + tailAngle

    points = L.map (polar arcCenter r << incrAngle) [ 0 .. res ]
  in GC.path points

midpoint (x1, y1) (x2, y2) = (0.5 * (x1 + x2), 0.5 * (y1 + y2))

length (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

rotationAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1) 

type alias Point = (Float, Float)

polar : Point -> Float -> Float -> Point
polar (x0, y0) r theta = 
  let
    x = r * (cos theta)
    y = r * (sin theta)
  in (x0 + x, y0 + y)