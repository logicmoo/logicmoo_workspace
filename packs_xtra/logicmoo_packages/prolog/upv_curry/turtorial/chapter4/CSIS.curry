import List
import Findall

-- 2001 catalog

isPrereqOf 162 = 161
isPrereqOf 163 = 162
isPrereqOf 200 = 162
isPrereqOf 201 = 200
isPrereqOf 202 = 163
isPrereqOf 202 = 201
isPrereqOf 250 = 163
isPrereqOf 251 = 250
isPrereqOf 252 = 251
isPrereqOf 300 = 202
isPrereqOf 301 = 252
isPrereqOf 301 = 300
isPrereqOf 302 = 301
isPrereqOf 303 = 252
isPrereqOf 303 = 300
isPrereqOf 350 = 252

allIsPrereqOf     course = findall (\p -> isPrereqOf course =:= p)
allGivesAccessTo  course = findall (\c -> isPrereqOf c =:= course)

transClosPrereq course = tail (listAll [course])
  where directList l = concat (map (\c -> findall (\p -> isPrereqOf c =:= p)) l)
        listAll l = if (tmp == []) then l else nub (l ++ listAll tmp)
          where tmp = directList l

main = transClosPrereq 202  -- -> [163,201,162,200,161]
