module Interface where

import Interface.System as System
import Interface.Clock as Clock

import Graphics.Collage as GC
import Graphics.Element as GE

display : (List (Int, String), List ((Int, Int), List Float)) -> Float -> GE.Element
display (stocks, flows) time =
  let
    clock = Clock.pendulum time
    systemDrawing = System.draw stocks flows
  in GE.flow GE.right [ systemDrawing, clock ]
