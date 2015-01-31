import Graphics.Element (Element)
import Signal (..)
import Text (plainText)
import Mouse
import Time

main : Signal Element
main = map display counter

display : Float -> Element
display t = plainText (toString (floor t))

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