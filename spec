a scalar is an unsigned number

a stock has
  [scalar] size
  [list of flows] output

a flow has
  [scalar] rate
  [queue of scalars] pipeline
  [stock] destination

given stocks and flows
every tick
  for each flow F
    F.pipeline >> p
    F.destination.size += p
  for each stock S
    for each S.output F
      a = min(F.rate, S.size)
      S.size -= a
      a >> F.pipeline


withdraw : Stock -> Scalar -> (Stock, Scalar)
withdraw s n =
  let
    x = s.balance
    dx = min n x
  in (setBalance s (x - dx), dx)


deposit : Stock -> Scalar -> Stock
deposit s dx =
  let
    x = s.balance
  in setBalance s (x + dx)


dequeue : Flow -> (Flow, Scalar)
dequeue f =
  let
    n:ns = f.pipeline
  in (setPipeline f ns, n)

enqueue : Flow -> Scalar -> Flow
enqueue f n =
  let
    ns = f.pipeline
  in setPipeline f (append ns n)
