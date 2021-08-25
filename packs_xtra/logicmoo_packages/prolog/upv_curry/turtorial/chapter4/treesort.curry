-------------------------------------------------------------------------
-- Definitions of binary search trees, inserting and in-order traversal:

data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)

insert x Leaf = Branch x Leaf Leaf
insert x (Branch d l r)
  | x < d = Branch d (insert x l) r
  | x > d = Branch d l (insert x  r)
  | otherwise = Branch d l r

inorder Leaf = []
inorder (Branch d l r) = inorder l ++ [d] ++ inorder r
-------------------------------------------------------------------------

testdata = [9,2,6,3,5,4,1,7,8,0]

main = inorder (list2bst testdata)
     where list2bst [] = Leaf
     	   list2bst (x:xs) = insert x (list2bst xs)
