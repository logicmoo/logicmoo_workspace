infixl 8 **
a ** b | b >= 0 = accum 1 a b
   where accum x y z | z == 0    = x
                     | otherwise = accum aux (y * y) (z `div` 2)
           where aux = if (z `mod` 2 == 1) then x * y else x


main = map (2**) [0,10,20,30,40]
-- expect: [1,1024,1048576,1073741824,1099511627776]
