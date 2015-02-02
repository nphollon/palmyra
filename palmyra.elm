import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Text (plainText)
import Mouse
import Time

main : Signal Element
main = map (pendulum 90 (degrees 20)) counter

pendulum : Float -> Float -> Float -> Element
pendulum r thetaMax t =
  let
    theta = thetaMax * cos (0.2 * pi * t)
    x = r * (sin <| theta)
    y = 40 - (r * (cos <| theta))
    clockLocation = (0, 40)
    bobLocation = (x, y)
    bob = filled red (circle 10)
    thread = traced (solid black) (segment clockLocation bobLocation)
  in collage 120 120 [ 
    thread,
    digitalClock t |> move clockLocation,
    bob |> move bobLocation ]

digitalClock : Float -> Form
digitalClock t =
  let ticks = t * 0.1 |> floor |> toString |> plainText |> toForm
  in group [ filled lightBlue (rect 50 20), ticks ] |> moveY 10

counter : Signal Float
counter =
  let rate = \t -> Time.inSeconds t * 10
  in accum rate 0 time

time : Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 100 pause

toggle : Bool -> Signal a -> Signal Bool
toggle = foldp (\_ b -> not b)

accum : (a -> number) -> number -> Signal a -> Signal number
accum f = foldp (\t c -> f t + c)
