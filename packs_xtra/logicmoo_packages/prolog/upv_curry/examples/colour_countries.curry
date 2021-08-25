-- Example about Concurrent Features in Curry

-- Solve the classical map coloring problem 

-- Check elements for incompatibility:
diff :: a -> a -> Constraint
diff x y = (x==y)=:=False

-- Define color data and generating relation
data Color = Red | Green | Yellow | Blue

isColor :: Color -> Constraint
isColor Red    = success
isColor Yellow = success
isColor Green  = success
isColor Blue   = success

-- Obtain valid colors for each country
coloring :: Color -> Color -> Color -> Color -> Constraint
coloring l1 l2 l3 l4 = isColor l1 & isColor l2 & isColor l3 & isColor l4

-- Check which countries must have different colors
correct :: Color -> Color -> Color -> Color -> Constraint
correct l1 l2 l3 l4 = diff l1 l2 & diff l1 l3 & diff l2 l3 & diff l2 l4 & diff l3 l4

-- Classical generate and test solution
generate_test l1 l2 l3 l4 = coloring l1 l2 l3 l4 & correct l1 l2 l3 l4

-- Faster solution
test_generate l1 l2 l3 l4 = correct l1 l2 l3 l4 & coloring l1 l2 l3 l4
