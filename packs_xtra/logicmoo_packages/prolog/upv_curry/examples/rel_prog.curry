-- Declaration of an enumeration type for persons:
-- (as an alternative, one could consider persons as strings)

data Person = Christine | Maria | Monica | Alice | Susan |
              Antony | Bill | John | Frank | Peter | Andrew

-- Two basic relationsships:

married :: Person -> Person -> Constraint
married Christine Antony = success
married Maria Bill       = success
married Monica John      = success
married Alice Frank      = success

mother :: Person -> Person -> Constraint
mother Christine John  = success
mother Christine Alice = success
mother Maria Frank     = success
mother Monica Susan    = success
mother Monica Peter    = success
mother Alice Andrew    = success

-- and here are the deduced relationships:
father :: Person -> Person -> Constraint
father f c = let m free in married m f & mother m c

grandfather :: Person -> Person -> Constraint
grandfather g c | father g f & father f c = success
                | father g m & mother m c = success
                where f,m free

