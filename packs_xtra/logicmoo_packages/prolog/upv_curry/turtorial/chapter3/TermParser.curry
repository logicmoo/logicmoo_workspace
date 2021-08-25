import Char

data Term = Term String [Term]

parseTerm s | s =:= fun++"("++args++")" &
              all isAlpha fun =:= True = Term fun (parseArgs args)
  where fun, args free
parseTerm s | all isAlpha s =:= True = Term s []

parseArgs s | s =:= term++","++terms &
              parseTerm term =:= result = result : parseArgs terms
  where term, terms, result free
parseArgs s | parseTerm s =:= result = [result]
  where result free


main1 = parseTerm "f(g(a,b))"
-- => Term "f" [Term "g" [Term "a" [],Term "b" []]]
main2 = parseTerm "zero"
-- => Term "zero" []

main = main1
