-- Matrix transposal with inductive definitions

transpose m = 
  let c = let fcol []         = []
              fcol ([]:_)     = []
              fcol ((u:_):v)  = u : fcol v
           in fcol m
      r = let rest []         = []
              rest ([]:_)     = [] 
              rest ((_:u):v)  = u : rest v
           in rest m
   in if r==[] then [] else c : transpose r

-- sample argument
matrix = [[1,0,2],[3,7,2],[2,8,1],[3,3,4]]
-- sample result
result = [[1,3,2,3],[0,7,8,3],[2,2,1,4]]
-- simple test
main = transpose matrix == result

