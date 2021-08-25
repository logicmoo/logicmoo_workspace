data Suit
  = Club 
  | Spade 
  | Heart
  | Diamond 
data Rank
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two 
data Card = Card Rank Suit

rank (Card r _) = r
suit (Card _ s) = s
