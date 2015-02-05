import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import List
import Signal
import Text (plainText)
import Mouse
import Time

type alias System = { time:Float, stocks:List Stock, flows:List Flow }
type alias Stock = { name:String, size:Int }
type alias Flow = { source:String, sink:String }

main : Signal.Signal Element
main = Signal.map display system

network = {
    time = 0,
    stocks = [ { name="start", size=500 }, { name="middle", size=20 }, { name="end", size=7 } ],
    flows = [ { source="start", sink="middle" }, { source="middle", sink="end" } ]
  }

system : Signal.Signal System
system = Signal.map (\t -> { network | time <- t }) counter


display : System -> Element
display sys =
  let
    clock = collage 120 130 [ pendulum 90 (degrees 20) sys.time |> moveY 45 ]
    stocks = drawStocks sys.stocks
    flows = drawFlows sys.flows
  in flow down [ clock, stocks, flows ]

pendulum : Float -> Float -> Float -> Form
pendulum r thetaMax t =
  let
    theta = thetaMax * cos (2 * pi * t)
    x = r * (sin theta)
    y = r * (cos theta) |> negate
    thread = traced (dotted black) (segment (0, 0) (x,y))
    bob = filled red (circle 10) |> move (x,y)

    ticks = t |> floor |> toString |> plainText |> toForm
    digitalClock = group [ filled lightBlue (rect 50 20), ticks ] |> moveY 10
  in group [ thread, digitalClock, bob ]

drawStocks : List Stock -> Element
drawStocks stocks = 
  let
    label stock = stock.name ++ "\n" ++ (toString stock.size)
    format string = plainText string |> (container 80 40 middle) |> color lightYellow
    draw = label >> format
  in List.map draw stocks |> List.intersperse (spacer 10 1) |> flow right

drawFlows : List Flow -> Element
drawFlows flows =
  let
    label flow = "from " ++ flow.source ++ " to " ++ flow.sink
    format string = plainText string |> (container 200 40 middle) |> color lightGreen
    draw = label >> format
  in List.map draw flows |> List.intersperse (spacer 10 1) |> flow right


counter : Signal.Signal Float
counter =
  let rate = \t -> Time.inSeconds t
  in accum rate 0 time

time : Signal.Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 100 pause

toggle : Bool -> Signal.Signal a -> Signal.Signal Bool
toggle = Signal.foldp (\_ b -> not b)

accum : (a -> number) -> number -> Signal.Signal a -> Signal.Signal number
accum f = Signal.foldp (\t c -> f t + c)

