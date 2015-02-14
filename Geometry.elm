module Geometry where

import Array as A
import List as L

arc : Float -> Point -> Point -> List Point
arc h head tail =
  let
    res = 100
    sign = abs h / h

    l = distance tail head
    r = (l^2 + 4 * h^2) / (8 * h)
    inclination = rotationAngle head tail - degrees 90

    mid = midpoint head tail
    arcCenter = polar mid (r - h) inclination
    headAngle = rotationAngle arcCenter head

    arcLength = 2 * atan2 (0.5 * l) (sign * (r - h))
    angle i = headAngle - sign * i * arcLength / res

    arcPoint = polar arcCenter (sign * r) << angle << toFloat

  in A.initialize res arcPoint |> A.toList

midpoint (x1, y1) (x2, y2) = (0.5 * (x1 + x2), 0.5 * (y1 + y2))

distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

rotationAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1) 

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
