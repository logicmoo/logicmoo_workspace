-- Example about Curry High Order Facilities

-- Quicksort function:
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs)
                   ++ [x]
                   ++ quicksort (filter (> x) xs)

