sort []     = []
sort (x:xs) = insert x (sort xs)

insert x [] = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y : insert x ys

main = sort [3,5,1,2,6,7,8,9]  -- -> [1,2,3,5,6,7,8,9]
