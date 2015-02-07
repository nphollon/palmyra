module Interface where

import Color as C
import Dict as D
import Graphics.Collage as GC
import Graphics.Element as GE
import List as L
import Text (plainText)

display sys time =
  let clock = GC.collage 120 130 [ pendulum time |> GC.moveY 45 ]
  in GE.flow GE.down [ clock, drawStocks (fst sys), drawFlows (snd sys) ]

pendulum t =
  let
    theta = (degrees 20) * cos (2 * pi * t)
    bobXY = polar 90 theta
    thread = GC.traced (GC.solid C.black) (GC.segment (0, 0) bobXY)
    bob = GC.filled C.red (GC.circle 10) |> GC.move bobXY

    ticks = t |> floor |> toString |> plainText |> GC.toForm
    digitalClock = GC.group [ GC.filled C.lightBlue (GC.rect 50 20), ticks ] |> GC.moveY 10
  in GC.group [ thread, digitalClock, bob ]

polar : Float -> Float -> (Float, Float)
polar r theta = 
  let
    x = r * (sin theta)
    y = r * (cos theta) |> negate
  in (x,y)

circlePositions n =
  let 
    nf = toFloat n
    angle i = degrees (i * 360 / nf)
  in L.map (polar 150 << angle) [ 1 .. nf ]

drawStocks stocks = 
  let labels = D.values stocks
  in labels
    |> L.map drawStock
    |> L.map2 GC.move (circlePositions <| L.length labels)
    |> GC.collage 500 500

drawStock label =
  plainText label 
  |> (GE.container 120 30 GE.middle)
  |> GE.color C.lightYellow
  |> GC.toForm

drawFlows flows = 
  L.map drawFlow flows 
  |> L.intersperse (GE.spacer 10 1) 
  |> GE.flow GE.right

drawFlow label =
  plainText label 
  |> (GE.container 160 30 GE.middle)
  |> GE.color C.lightGreen
