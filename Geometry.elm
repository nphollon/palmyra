module Geometry where

import Array as A
import List as L

arc : Float -> Point -> Point -> List Point
arc h tail head =
  let
    res = 100
    mid = midpoint head tail
    l = distance tail head
    r = (l^2 + 4 * h^2) / (8 * h)
    inclination = rotationAngle tail head + degrees 90
    arcCenter = polar mid (r-h) inclination

    arcLength = 2 * atan2 (0.5 * l) (r - h)
    tailAngle = rotationAngle arcCenter tail
    arcAngle i = arcLength * i / res + tailAngle
    arcPoint = polar arcCenter r << arcAngle << toFloat

  in A.initialize res arcPoint |> A.toList

midpoint (x1, y1) (x2, y2) = (0.5 * (x1 + x2), 0.5 * (y1 + y2))

distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

rotationAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1) 

rotate : Float -> Point -> Point -> Point
rotate a c p = polar c (distance c p) (a + rotationAngle c p)

type alias Point = (Float, Float)

polar : Point -> Float -> Float -> Point
polar (x0, y0) r theta = 
  let
    x = r * (cos theta)
    y = r * (sin theta)
  in (x0 + x, y0 + y)

isWithin : Float -> Point -> Point -> Bool
isWithin radius origin point = distance origin point <= radius

arrowhead : Float -> Float -> Point -> Point -> List Point
arrowhead r width p1 dir =
  let
    incline = rotationAngle p1 dir
    p2 = polar p1 r (incline + 0.5 * width)
    p3 = polar p1 r (incline - 0.5 * width)
  in [ p1, p2, p3 ]
