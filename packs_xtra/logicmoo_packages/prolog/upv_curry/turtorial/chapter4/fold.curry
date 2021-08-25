import Integer

sumList = foldr (+) 0
prodList = foldr (*) 1
maxList = \l -> foldr max (head l) (tail l)

main1 = sumList  [1,2,3,4,5]  -- ->  15
main2 = prodList [1,2,3,4,5]  -- -> 120
main3 = maxList  [1,2,3,4,5]  -- ->   5

main = main2

