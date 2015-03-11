module Data.CompoundInterest (bankAccount) where 

import System.Stock (Stock(..))
import System.Flow (Flow(..))
import Dict


bankAccount = {
    stocks = Dict.fromList [("Account", Mass 100)],
    flows = Dict.fromList [("Interest", Growth "Account" 0.0003)]
  }
