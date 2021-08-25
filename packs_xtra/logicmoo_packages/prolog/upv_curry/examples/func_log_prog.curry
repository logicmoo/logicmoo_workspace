-- Example about Functional Logic  Programming

pragma flex

data Person = Christine | Maria | Monica | Alice | Susan |
              Antony | Bill | John | Frank | Peter | Andrew

-- Two basic functional dependencies:

husband :: Person -> Person
husband Christine = Antony
husband Maria     = Bill  
husband Monica    = John  
husband Alice     = Frank 

mother :: Person -> Person
mother John   = Christine
mother Alice  = Christine
mother Frank  = Maria    
mother Susan  = Monica   
mother Peter  = Monica   
mother Andrew = Alice    

-- and here are the deduced functions and relationships:
father :: Person -> Person
father c = husband (mother c)

grandfather :: Person -> Person
grandfather g = father (father g)
grandfather g = father (mother g)
