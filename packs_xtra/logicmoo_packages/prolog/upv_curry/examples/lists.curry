-- Example about Lists

-- Concatenation of two lists:
append :: [t] -> [t] -> [t]
append eval flex
append []     ys = ys
append (x:xs) ys = x:append xs ys

-- Naive reverse of all llists elements:
rev :: [t] -> [t]
rev []     = []
rev (x:xs) = append (rev xs) [x]

-- Last element of a list:
last :: [t] -> t
last xs | append _ [x] =:= xs = x where x free
