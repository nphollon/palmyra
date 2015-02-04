a scalar is an unsigned number
a vector is a signed number

a stock has
  [scalar] size
  [list of flows] output

a flow has
  [vector] rate
  [queue of scalars] pipeline
  [stock] destination

given stocks and flows
every tick
  for each flow F
    F.pipeline >> p
    F.destination.size += size
  for each stock S
    for each S.output F
      a = min(F.rate, S.size)
      S.size -= a
      a >> F.pipeline
