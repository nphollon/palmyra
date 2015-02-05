module Interface where

import System (..)

import Color as C
import Graphics.Element as GE
import Graphics.Collage as GC
import List as L
import Text (plainText)

display : System -> GE.Element
display sys =
  let
    clock = GC.collage 120 130 [ pendulum 90 (degrees 20) sys.time |> GC.moveY 45 ]
    stocks = drawStocks sys.stocks
    flows = drawFlows sys.flows
  in GE.flow GE.down [ clock, stocks, flows ]

pendulum : Float -> Float -> Float -> GC.Form
pendulum r thetaMax t =
  let
    theta = thetaMax * cos (2 * pi * t)
    x = r * (sin theta)
    y = r * (cos theta) |> negate
    thread = GC.traced (GC.dotted C.black) (GC.segment (0, 0) (x,y))
    bob = GC.filled C.red (GC.circle 10) |> GC.move (x,y)

    ticks = t |> floor |> toString |> plainText |> GC.toForm
    digitalClock = GC.group [ GC.filled C.lightBlue (GC.rect 50 20), ticks ] |> GC.moveY 10
  in GC.group [ thread, digitalClock, bob ]

drawStocks : List Stock -> GE.Element
drawStocks stocks = 
  let
    label stock = stock.name ++ "\n" ++ (toString stock.size)
    format string = plainText string |> (GE.container 80 40 GE.middle) |> GE.color C.lightYellow
    draw = label >> format
  in L.map draw stocks |> L.intersperse (GE.spacer 10 1) |> GE.flow GE.right

drawFlows : List Flow -> GE.Element
drawFlows flows =
  let
    label flow = "from " ++ flow.source ++ " to " ++ flow.sink
    format string = plainText string |> (GE.container 200 40 GE.middle) |> GE.color C.lightGreen
    draw = label >> format
  in L.map draw flows |> L.intersperse (GE.spacer 10 1) |> GE.flow GE.right