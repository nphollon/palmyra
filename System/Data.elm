module System.Data where

import System.Stock (..)

type Component = Node Int String | Arc Int Int String

stockNode : Stock -> Component
stockNode