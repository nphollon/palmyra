module Data.CompoundInterest (bankAccount) where 

import System.Stock (Stock(..))
import Dict

interestRate = 0.03

bankAccount = {
    stocks = Dict.fromList [(1, Mass "Account" 100), (2, Ground "Interest")],
    flows = [ interestFlow ]
  }

interestFlow = {
    source = 2, sink = 1, state = 0, states = Dict.singleton 0 accruingInterest
  }

accruingInterest = 
  let flux _ principal = interestRate * principal
  in { flux=flux, rules=[] }
