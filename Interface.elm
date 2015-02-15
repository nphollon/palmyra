module Interface where

import Interface.System as I2
import System.Data as SD

import Color as C
import Dict as D
import Graphics.Collage as GC
import Graphics.Element as GE
import List as L
import Maybe as M
import Text as T

display : (List (Int, String), List (Int, Int)) -> Float -> GE.Element
display (stocks, flows) time =
  let
    clock = GC.collage 120 130 [ pendulum time |> GC.moveY 45 ]
    systemDrawing = I2.drawSystem stocks flows |> GC.collage 500 500 
  in GE.flow GE.down [ clock, systemDrawing ]

pendulum t =
  let
    theta = (degrees 20) * cos (2 * pi * t)
    bobXY = polar 90 theta
    thread = GC.traced (GC.solid C.black) (GC.segment (0, 0) bobXY)
    bob = GC.filled C.red (GC.circle 10) |> GC.move bobXY

    ticks = t |> floor |> toString |> T.plainText |> GC.toForm
    digitalClock = GC.group [ GC.filled C.lightBlue (GC.rect 50 20), ticks ] |> GC.moveY 10
  in GC.group [ thread, digitalClock, bob ]

polar : Float -> Float -> (Float, Float)
polar r theta = 
  let
    x = r * (sin theta)
    y = r * (cos theta) |> negate
  in (x,y)
