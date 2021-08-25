data Tree a = Tree a [Tree a]

type Trie = [Tree Char]

insert :: String -> Trie -> Trie
insert [] t = (Tree '.' [] : t)
insert (w:ws) [] = [Tree w (insert ws [])]
insert (w:ws) (Tree c cs : ts)
  | ord w < ord c = insert (w:ws) [] ++ (Tree c cs : ts)
  | ord w > ord c = Tree c cs : insert (w:ws) ts
  | otherwise = Tree c (insert ws cs) : ts

main = insert "car" (insert "care" (insert "carton" []))
