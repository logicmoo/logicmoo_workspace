sort _ []     = []
sort f (x:xs) = insert f x (sort f xs)

insert _ x [] = [x]
insert f x (y:ys) | f x y    = x:y:ys
                  | otherwise = y : insert f x ys

main1 = sort (<=) [3,5,1,2,6,7,8,9]  -- -> [1,2,3,5,6,7,8,9]
main2 = sort (>) [3,5,1,2,6,7,8,9]  -- -> [9,8,7,6,5,3,2,1]

main = main2
