module Types where

type alias System = { time:Float, stocks:List Stock, flows:List Flow }
type alias Stock = { name:String, size:Int }
type alias Flow = { source:String, sink:String }