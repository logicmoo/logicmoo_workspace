-- Subtract the minumum element of a list
-- from each element of the list in 1 pass.
-- There also a solution based on narrowing.

import Integer

submin [] = []
submin (x:xs) = fst (aux (x:xs) x)
  where aux []     m = ([],m)
        aux (y:ys) m = let (zs,n) = aux ys (min y m)
                        in (y-n:zs,n)


main = submin [3,5,2,6,7]
-- -> [1,3,0,4,5]
