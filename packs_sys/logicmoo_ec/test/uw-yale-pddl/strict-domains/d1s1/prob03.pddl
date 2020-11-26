( DEFINE ( PROBLEM D1S1-3 )
    (:domain
  D1S1)
  (:init
   ( I2 ) 
   ( I3 ) 
   ( I1 ) 
   ) 
  (:goal
  ( AND
    ( G1 ) 
    ( G3 ) 
    ( G2 ) 
    ))
  (:length (:serial 3) (:parallel 3))
  )