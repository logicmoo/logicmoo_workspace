( DEFINE ( PROBLEM D1S1-6 )
    (:domain
    D1S1)
    (:init
        ( I1 ) 
        ( I3 ) 
        ( I4 ) 
        ( I5 ) 
        ( I6 ) 
        ( I2 ) 
     ) 
    (:goal
    ( AND
        ( G2 ) 
        ( G1 ) 
        ( G4 ) 
        ( G6 ) 
        ( G3 ) 
        ( G5 ) 
	) )
    (:length (:serial 6) (:parallel 6)) 
 )