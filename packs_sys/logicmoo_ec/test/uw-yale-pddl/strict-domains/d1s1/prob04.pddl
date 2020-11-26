( DEFINE ( PROBLEM D1S1-4 )
    (:domain
    D1S1)
    (:init
        ( I2 ) 
        ( I4 ) 
        ( I1 ) 
        ( I3 ) 
     ) 
    (:goal
    ( AND
        ( G2 ) 
        ( G4 ) 
        ( G3 ) 
        ( G1 ) 
	) )
    (:length (:serial 4) (:parallel 4))
 )