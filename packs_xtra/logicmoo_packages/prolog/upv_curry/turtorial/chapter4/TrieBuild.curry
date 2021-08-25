import SetRBT

---------------------------------------------------------------------
-- Definitions from TrieInsert:

data Tree a = Tree a [Tree a]

type Trie = [Tree Char]

insert :: String -> Trie -> Trie
insert [] t = (Tree '.' [] : t)
insert (w:ws) [] = [Tree w (insert ws [])]
insert (w:ws) (Tree c cs : ts)
  | ord w < ord c = insert (w:ws) [] ++ (Tree c cs : ts)
  | ord w > ord c = Tree c cs : insert (w:ws) ts
  | otherwise = Tree c (insert ws cs) : ts
---------------------------------------------------------------------

-- A set of words
sample = [
  "aback",
  "abaft",
  "abandon",
  "abandoned",
  "abandoning",
  "abandonment",
  "abandons",
  "abase",
  "abased",
  "abasement",
  "abasements",
  "abases",
  "abash",
  "abashed",
  "abashes",
  "abashing",
  "zonal",
  "zonally",
  "zone",
  "zoned",
  "zones",
  "zoning",
  "zoo",
  "zoological",
  "zoologically",
  "zoom",
  "zooms"]

-- the same set in scrambled order
scrambled = map reverse (sortRBT precede (map reverse sample))
  where precede [] _ = True
        precede (_:_) [] = False
        precede (x:xs) (y:ys) 
           | ord x < ord y = True
           | ord y < ord x = False
           | otherwise     = precede xs ys

-- build a trie from a list of words:
buildTrie :: [String] -> Trie
buildTrie wordList = foldr (\x y -> insert x y) [] wordList

-- print the trie
pp trie = ppaux "" trie
  where ppaux s (Tree c []) | c == '.'  = [s]
                            | otherwise = []
        ppaux s (Tree c (x:xs)) = ppaux (s++[c]) x ++ ppaux s (Tree c xs)

-- build and print all the words in a trie
-- a share prefix is printed for each word sharing it
main = foldr (\x y -> pp x ++ y) [] (buildTrie scrambled)
