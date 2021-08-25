import Findall

----------------------------------------------------------------------
-- imported from Cards:
data Suit = Club | Spade | Heart | Diamond 
data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight
          | Seven | Six | Five | Four | Three | Two 
data Card = Card Rank Suit

rank (Card r _) = r
suit (Card _ s) = s
----------------------------------------------------------------------

fourConstraint hand | hand =:= x++y:z & map rank (x++z) =:= [r,r,r,r]
                    = r
                    where x,y,z,r free

isFour hand = putStrLn (if sorry then "Sorry" else "Four "++(show rank1))
            where score = findall (\r -> fourConstraint hand =:= r)
                  sorry = score == []
                  rank1 = head score

testYes = [(Card Six Club),(Card Six Spade),(Card Five Heart),
           (Card Six Heart),(Card Six Diamond)]
testNo  = [(Card Six Club),(Card Ace Spade),(Card Five Heart),
           (Card Ace Club),(Card Six Diamond)]

-- fourConstraint  testNo  => no solution
-- fourConstraint  testYes => Six

-- isFour testNo  => Sorry
-- isFour testYes => Four Six

main = isFour testYes
