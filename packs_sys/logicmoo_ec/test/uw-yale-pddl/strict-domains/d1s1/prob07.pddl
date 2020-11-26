( DEFINE ( PROBLEM D1S1-7 )
    (:domain
    D1S1)
    (:init
        ( I3 ) 
        ( I6 ) 
        ( I5 ) 
        ( I1 ) 
        ( I4 ) 
        ( I2 ) 
        ( I7 ) 
     ) 
    (:goal
    ( AND
        ( G3 ) 
        ( G6 ) 
        ( G5 ) 
        ( G7 ) 
        ( G1 ) 
        ( G4 ) 
        ( G2 ) 
	) )
    (:length (:serial 7) (:parallel 7))
 )