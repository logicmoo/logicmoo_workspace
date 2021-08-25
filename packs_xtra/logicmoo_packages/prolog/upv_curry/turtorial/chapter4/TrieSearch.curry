---------------------------------------------------------------------
-- Definitions from TrieBuild:

data Tree a = Tree a [Tree a]

type Trie = [Tree Char]

insert :: String -> Trie -> Trie
insert [] t = (Tree '.' [] : t)
insert (w:ws) [] = [Tree w (insert ws [])]
insert (w:ws) (Tree c cs : ts)
  | ord w < ord c = insert (w:ws) [] ++ (Tree c cs : ts)
  | ord w > ord c = Tree c cs : insert (w:ws) ts
  | otherwise = Tree c (insert ws cs) : ts

-- build a trie from a list of words:
buildTrie :: [String] -> Trie
buildTrie wordList = foldr (\x y -> insert x y) [] wordList
---------------------------------------------------------------------

-- Is a word in a trie?
isin [] (Tree c _ : _) = c == '.'
isin (_:_) [] = False
isin (w:ws) (Tree c cs : ts)
  | ord w < ord c = False
  | ord w > ord c = isin (w:ws) ts
  | otherwise = isin ws cs

main = isin "care" (buildTrie ["car", "care", "carton"])

