import Interface (display)
import System

import Graphics.Element (Element)
import Signal as S
import Mouse
import Time


main : S.Signal Element
main = S.map display systemSignal

network = System.stock "start" 500
  |> System.flowTo "middle" 20
  |> System.flowTo "end" 7

systemSignal : S.Signal System.System
systemSignal =
  let updateTime system dt = { system | time <- dt }
  in S.map (updateTime network) counter


counter : S.Signal Float
counter =
  let rate = \t -> Time.inSeconds t
  in accum rate 0 time

time : S.Signal Time.Time
time = let pause = toggle True Mouse.clicks
  in Time.fpsWhen 60 pause

toggle : Bool -> S.Signal a -> S.Signal Bool
toggle = S.foldp (\_ b -> not b)

accum : (a -> number) -> number -> S.Signal a -> S.Signal number
accum f = S.foldp (\t c -> f t + c)