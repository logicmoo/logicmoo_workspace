slowRev [] = []
slowRev (u:v) = slowRev v ++ [u]
fastRev l = aux l []
  where aux [] r = r
        aux (u:v) r = aux v (u:r)
