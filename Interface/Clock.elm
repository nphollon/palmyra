module Interface.Clock where

import Interface.Geometry as Geo

import Color as C
import Graphics.Collage as GC
import Graphics.Element (Element)
import Text as T

pendulum : Float -> Element
pendulum t =
  let
    pivot = (0, 100)
    theta = (degrees 5) * cos (2 * pi * t)
    bobXY = Geo.polar pivot 200 (theta - degrees 90)
    thread = GC.traced (GC.solid C.black) (GC.segment pivot bobXY)
    bob = GC.filled C.red (GC.circle 15) |> GC.move bobXY

    ticks = t |> floor |> toString |> T.plainText |> GC.toForm
    digitalClock = GC.group [ GC.filled C.green (GC.rect 100 50), ticks ] |> GC.moveY 125
  in GC.collage 100 300 [ thread, digitalClock, bob ]

