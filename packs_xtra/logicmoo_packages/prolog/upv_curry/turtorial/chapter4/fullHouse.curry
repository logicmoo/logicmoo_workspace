----------------------------------------------------------------------
--import Cards
-- imported from Cards:
data Suit = Club | Spade | Heart | Diamond 
data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight
          | Seven | Six | Five | Four | Three | Two 
data Card = Card Rank Suit

rank (Card r _) = r
suit (Card _ s) = s
----------------------------------------------------------------------

fullHouseConstraint hand | hand =:= x++u:y++v:z
                         & map rank (x++y++z) =:= [r,r,r]
                         & rank u =:= rank v
                         = (r,rank u)
                         where x,y,z,u,v,r free

testYes = [(Card Ace Club),(Card Two Spade),(Card Ace Heart),
           (Card Two Heart),(Card Two Diamond)]
testNo  = [(Card Six Club),(Card Ace Spade),(Card Five Heart),
           (Card Ace Club),(Card Six Diamond)]

-- fullHouseConstraint testNo  => no solution
-- fullHouseConstraint testYes => (Two,Ace)

main = fullHouseConstraint testYes

