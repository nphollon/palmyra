import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Text (plainText)
import Mouse
import Time

main : Signal Element
main = map display counter

display : Float -> Element
display t =
  let
    ticks = t * 0.1 |> floor |> toString |> plainText |> toForm
    clockLocation = (0, 40)
    bobLocation = (25 * (cos (0.2 * pi * t)), 3.791713 * (cos (0.4 * pi * t)) - 46.208)
    digitalClock = group [ filled lightBlue (rect 50 20), ticks ]
    bob = filled red (circle 10)
    thread = traced (solid black) (segment clockLocation bobLocation)
  in collage 120 120 [ 
    thread,
    digitalClock |> move (0, 50),
    bob |> move bobLocation ]

counter : Signal Float
counter =
  let rate = \t -> Time.inMilliseconds t * 0.01
  in accum rate 0 time

time : Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 60 pause

toggle : Bool -> Signal a -> Signal Bool
toggle = foldp (\_ b -> not b)

accum : (a -> number) -> number -> Signal a -> Signal number
accum f = foldp (\t c -> f t + c)