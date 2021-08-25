-- Compute a sublist l of the argument with the constrain
-- that the last element of l is twice the first.
-- E.g., the expected results of the test data are [3,6] and [2,1,4]

consublist (_++[x]++y++[z]++_) | 2*x == z = [x]++y++[z]

testdata = [3,6,2,1,4,5]

main = consublist testdata
