sumList :: [Int] -> Int
sumList []    = 0
sumList (u:v) = u + sumList v

main = sumList [0,1,2,3,4,5,6,7,8,9] -- -> 45
