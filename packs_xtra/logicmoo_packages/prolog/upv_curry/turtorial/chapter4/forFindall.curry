import Findall

subset []     = []
subset (x:xs) = x:subset xs
subset (_:xs) =   subset xs

allSubsets set = findall (\x -> subset set =:= x)

main = allSubsets [1,2,3]  -- -> [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
