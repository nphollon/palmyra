module Interface where

import System.Data as SD

import Color as C
import Dict as D
import Graphics.Collage as GC
import Graphics.Element as GE
import List as L
import Maybe as M
import Text as T

display : List SD.Component -> Float -> GE.Element
display components time =
  let
    clock = GC.collage 120 130 [ pendulum time |> GC.moveY 45 ]
  in GE.flow GE.down [ clock, drawSystem components ]

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

drawStock label =
  T.plainText label 
  |> (GE.container 120 30 GE.middle)
  |> GE.color C.lightYellow
  |> GC.toForm

drawSystem : List SD.Component -> GE.Element
drawSystem components =
  let
    nodes = L.filter SD.isNode components
    dt = (/) (degrees 360) <| toFloat <| L.length nodes
    angleForNode node (t, tDict) = (t + dt, D.insert (SD.id node) t tDict)
    angles = L.foldl angleForNode (0, D.empty) nodes |> snd
  in L.map (drawComponent angles) components |> GC.collage 500 500

drawComponent : D.Dict Int Float -> SD.Component -> GC.Form
drawComponent angles component =
  let
    getPoint r i = D.get i angles |> M.withDefault 0 |> polar r
  in case component of
    SD.Node i s -> GC.move (getPoint 150 i) (drawStock s)
    SD.Arc i j _ -> GC.segment (getPoint 150 i) (getPoint 150 j) |> GC.traced (GC.solid C.black)
