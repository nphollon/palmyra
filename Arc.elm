module Arc (arc) where

import Graphics.Collage as GC
import List as L

arc res h tail head =
  let
    mid = midpoint head tail
    l = length tail head
    r = (l^2 + 4 * h^2) / (8 * h)
    inclination = rotationAngle tail head + degrees 90
    arcCenter = polar mid (r-h) inclination

    arcLength = 2 * atan2 (0.5 * l) (r - h)
    tailAngle = rotationAngle arcCenter tail
    incrAngle i = arcLength * i / res + tailAngle

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