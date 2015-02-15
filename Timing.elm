module Timing where

import Signal
import Time
import Mouse

discrete : Float -> Float -> Int
discrete plyPerSecond t = t * plyPerSecond |> floor

time : Float -> Signal Float
time dilation = Signal.foldp (tick dilation) 0 (Time.fpsWhen 33 pause)

pause : Signal Bool
pause = Signal.foldp (always not) True Mouse.clicks

tick : Float -> Time.Time -> Float -> Float
tick dilation dt t = Time.inSeconds dt * dilation + t