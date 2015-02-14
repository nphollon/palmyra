module CompoundInterest (bankAccount) where 

type System = { stocks: Dict Id Stock, flows: List Flow }

type Stock = { name: String, value: Scalar }

type Flow = { source: Id, sink: Id, state: Id, states: Dict Id State }
type State = { flux: Flux, rules: List (Trigger, Id) }

type alias Flux = Amount -> Amount -> Rate
type alias Trigger = Amount -> Amount -> Bool

type alias Id = Int

type Scalar = Ground | Mass Amount

type Amount = As Float
type GrowthFactor = PerPly Float
type Folding = Ply Float
type Rate = AsPerPly Float

interestRate = PerPly 0.03

bankAccount = {
    stocks = Dict.fromList [(1, "Account"), (2, "Interest")],
    flows = [ interestFlow ]
  }

interestFlow = {
    source = 2, sink = 1,
    state = 0, states = Dict.singleton 0 accruingInterest
  }

accruingInterest = 
  let flux _ principal = interestRate * principal
  in { flux=flux, rules=[] }
