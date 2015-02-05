import Interface (display)
import Types (..)

import Graphics.Element (Element)
import Signal as S
import Mouse
import Time


main : S.Signal Element
main = S.map display system

network = {
    time = 0,
    stocks = [ { name="start", size=500 }, { name="middle", size=20 }, { name="end", size=7 } ],
    flows = [ { source="start", sink="middle" }, { source="middle", sink="end" } ]
  }

system : S.Signal System
system = S.map (\t -> { network | time <- t }) counter


counter : S.Signal Float
counter =
  let rate = \t -> Time.inSeconds t
  in accum rate 0 time

time : S.Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 100 pause

toggle : Bool -> S.Signal a -> S.Signal Bool
toggle = S.foldp (\_ b -> not b)

accum : (a -> number) -> number -> S.Signal a -> S.Signal number
accum f = S.foldp (\t c -> f t + c)

