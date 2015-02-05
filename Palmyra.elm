import Interface
import System

import Signal ((<~), foldp)
import Mouse
import Time


main = Interface.display <~ systemSignal

systemSignal = foldp System.evolve startState intervals

startState = System.stock "start" 500
  |> System.flowTo "middle" 20
  |> System.flowTo "end" 7

intervals = Time.inSeconds <~ Time.fpsWhen 60 pause

pause = foldp (\_ b -> not b) False Mouse.clicks
