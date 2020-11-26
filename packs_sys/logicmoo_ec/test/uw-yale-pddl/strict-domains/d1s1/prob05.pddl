( DEFINE ( PROBLEM D1S1-5 )
    (:domain
  D1S1)
  (:init
   ( I2 ) 
   ( I1 ) 
   ( I4 ) 
   ( I5 ) 
   ( I3 ) 
   ) 
  (:goal
  ( AND
    ( G5 ) 
    ( G4 ) 
    ( G1 ) 
    ( G3 ) 
    ( G2 ) 
    ))
  (:length (:serial 5) (:parallel 5))
  )