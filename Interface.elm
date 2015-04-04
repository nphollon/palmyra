module Interface where

import System (..)

import Dict
import Graphics.Element (Element)
import List
import String as Str
import Text


draw : List (String, Float -> String) -> System -> Float -> Element
draw displayParams system _ =
  List.map (render system) displayParams |> printList

render : System -> (String, Float -> String) -> String
render system (label, format) =
  let maybeFormat mx = case mx of
    Just x -> format x
    Nothing -> "Nothing"
  in label ++ " : " ++ (view label system |> maybeFormat)

printList : List String -> Element
printList = Str.join "\n" >> Text.plainText