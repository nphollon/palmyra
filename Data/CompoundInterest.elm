module Data.CompoundInterest (bankAccount) where 

import System.Stock (Stock(..))
import Dict

interestRate = 0.0003

bankAccount = {
    stocks = Dict.fromList [("Account", Mass "Account" 100), ("Interest", Ground "Interest")],
    flows = [ interestFlow ]
  }

interestFlow = {
    source = 2, sink = 1, stateId = 0, states = Dict.singleton 0 accruingInterest
  }

accruingInterest = 
  let flux _ sink =
    case sink of
      Nothing -> 0
      Just principal -> interestRate * principal
  in { flux=flux, rules=[] }
