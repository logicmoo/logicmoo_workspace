-- Define a predicate, read as "factors" and denoted by
-- the infix symbol ./., that tells whether an
-- integer is a factor of another integer.
-- The operator is non-associative and has precedence 7.
-- The predicate should work for every input and 0
-- should not be a factor of any integer.

infix 7 ./.
a ./. b = a /= 0 && b `mod` a == 0

-- 1 ./. 6 => True
-- 2 ./. 7 => False
-- 0 ./. 8 => False

main = 3 ./. 15 -- -> True
