module Interface where

import Color as C
import Graphics.Element as GE
import Graphics.Collage as GC
import List as L
import Text (plainText)

display { time, stocks, flows } =
  let clock = GC.collage 120 130 [ pendulum time |> GC.moveY 45 ]
  in GE.flow GE.down [ clock, drawStocks stocks, drawFlows flows ]

pendulum t =
  let
    r = 90
    theta = (degrees 20) * cos (2 * pi * t)
    x = r * (sin theta)
    y = r * (cos theta) |> negate
    thread = GC.traced (GC.dotted C.black) (GC.segment (0, 0) (x,y))
    bob = GC.filled C.red (GC.circle 10) |> GC.move (x,y)

    ticks = t |> floor |> toString |> plainText |> GC.toForm
    digitalClock = GC.group [ GC.filled C.lightBlue (GC.rect 50 20), ticks ] |> GC.moveY 10
  in GC.group [ thread, digitalClock, bob ]

drawStocks stocks = 
  let
    label stock = stock.name ++ "\n" ++ (toString stock.size)
    format string = plainText string |> (GE.container 80 40 GE.middle) |> GE.color C.lightYellow
    draw = label >> format
  in L.map draw stocks |> L.intersperse (GE.spacer 10 1) |> GE.flow GE.right

drawFlows flows =
  let
    label flow = "from " ++ flow.source ++ " to " ++ flow.sink
    format string = plainText string |> (GE.container 200 40 GE.middle) |> GE.color C.lightGreen
    draw = label >> format
  in L.map draw flows |> L.intersperse (GE.spacer 10 1) |> GE.flow GE.right