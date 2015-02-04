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
display t = collage 120 130 [ pendulum 90 (degrees 20) t |> moveY 45 ]

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

counter : Signal Float
counter =
  let rate = \t -> Time.inSeconds t
  in accum rate 0 time

time : Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 100 pause

toggle : Bool -> Signal a -> Signal Bool
toggle = foldp (\_ b -> not b)

accum : (a -> number) -> number -> Signal a -> Signal number
accum f = foldp (\t c -> f t + c)
