;; domain D1S1
;; problem D1S1-1
(IN-PACKAGE "DOMAINS")
( DEFINE ( DOMAIN D1S1 ) 
    (:requirements :adl)
  
  (:predicates (I0)(I1)(I2)(I3)(I4)(I5)(I6)(I7)(I8)(I9)
	       (I10)(I11)(I12)(I13)(I14)(I15)(I16)(I17)(I18)(I19)(I20)
	       (G1)(G2)(G3)(G4)(G5)(G6)(G7)(G8)(G9)
	       (G10)(G11)(G12)(G13)(G14)(G15)(G16)(G17)(G18)(G19)(G20))



    ( :action A20
        :parameters
        ( ) 
        :precondition
        ( I20 ) 
        :effect
        ( AND 
            ( NOT
                ( I19 ) 
             ) 
            ( G20 ) 
         ) 
     )
    ( :action A19
        :parameters
        ( ) 
        :precondition
        ( I19 ) 
        :effect
        ( AND 
            ( NOT
                ( I18 ) 
             ) 
            ( G19 ) 
         ) 
     )
    ( :action A18
        :parameters
        ( ) 
        :precondition
        ( I18 ) 
        :effect
        ( AND 
            ( NOT
                ( I17 ) 
             ) 
            ( G18 ) 
         ) 
     )
    ( :action A17
        :parameters
        ( ) 
        :precondition
        ( I17 ) 
        :effect
        ( AND 
            ( NOT
                ( I16 ) 
             ) 
            ( G17 ) 
         ) 
     )
    ( :action A16
        :parameters
        ( ) 
        :precondition
        ( I16 ) 
        :effect
        ( AND 
            ( NOT
                ( I15 ) 
             ) 
            ( G16 ) 
         ) 
     )
    ( :action A15
        :parameters
        ( ) 
        :precondition
        ( I15 ) 
        :effect
        ( AND 
            ( NOT
                ( I14 ) 
             ) 
            ( G15 ) 
         ) 
     )
    ( :action A14
        :parameters
        ( ) 
        :precondition
        ( I14 ) 
        :effect
        ( AND 
            ( NOT
                ( I13 ) 
             ) 
            ( G14 ) 
         ) 
     )
    ( :action A13
        :parameters
        ( ) 
        :precondition
        ( I13 ) 
        :effect
        ( AND 
            ( NOT
                ( I12 ) 
             ) 
            ( G13 ) 
         ) 
     )
    ( :action A12
        :parameters
        ( ) 
        :precondition
        ( I12 ) 
        :effect
        ( AND 
            ( NOT
                ( I11 ) 
             ) 
            ( G12 ) 
         ) 
     )
    ( :action A11
        :parameters
        ( ) 
        :precondition
        ( I11 ) 
        :effect
        ( AND 
            ( NOT
                ( I10 ) 
             ) 
            ( G11 ) 
         ) 
     )
    ( :action A10
        :parameters
        ( ) 
        :precondition
        ( I10 ) 
        :effect
        ( AND 
            ( NOT
                ( I9 ) 
             ) 
            ( G10 ) 
         ) 
     )
    ( :action A9
        :parameters
        ( ) 
        :precondition
        ( I9 ) 
        :effect
        ( AND 
            ( NOT
                ( I8 ) 
             ) 
            ( G9 ) 
         ) 
     )
    ( :action A8
        :parameters
        ( ) 
        :precondition
        ( I8 ) 
        :effect
        ( AND 
            ( NOT
                ( I7 ) 
             ) 
            ( G8 ) 
         ) 
     )
    ( :action A7
        :parameters
        ( ) 
        :precondition
        ( I7 ) 
        :effect
        ( AND 
            ( NOT
                ( I6 ) 
             ) 
            ( G7 ) 
         ) 
     )
    ( :action A6
        :parameters
        ( ) 
        :precondition
        ( I6 ) 
        :effect
        ( AND 
            ( NOT
                ( I5 ) 
             ) 
            ( G6 ) 
         ) 
     )
    ( :action A5
        :parameters
        ( ) 
        :precondition
        ( I5 ) 
        :effect
        ( AND 
            ( NOT
                ( I4 ) 
             ) 
            ( G5 ) 
         ) 
     )
    ( :action A4
        :parameters
        ( ) 
        :precondition
        ( I4 ) 
        :effect
        ( AND 
            ( NOT
                ( I3 ) 
             ) 
            ( G4 ) 
         ) 
     )
    ( :action A3
        :parameters
        ( ) 
        :precondition
        ( I3 ) 
        :effect
        ( AND 
            ( NOT
                ( I2 ) 
             ) 
            ( G3 ) 
         ) 
     )
    ( :action A2
        :parameters
        ( ) 
        :precondition
        ( I2 ) 
        :effect
        ( AND 
            ( NOT
                ( I1 ) 
             ) 
            ( G2 ) 
         ) 
     )
    ( :action A1
        :parameters
        ( ) 
        :precondition
        ( I1 ) 
        :effect
        ( AND 
            ( NOT
                ( I0 ) 
             ) 
            ( G1 ) 
         ) 
     )
 ) 

( DEFINE ( PROBLEM D1S1-1 )
    (:domain
  D1S1)
  (:init
   ( I1 ) 
   ) 
  (:goal
  ( AND
    ( G1 ) 
    ))
  (:length (:serial 1) (:parallel 1))
  ) 

( DEFINE ( PROBLEM D1S1-2 )
    (:domain
    D1S1)
    (:init
        ( I1 ) 
        ( I2 ) 
     ) 
    (:goal
    ( AND
        ( G1 ) 
        ( G2 ) 
	) )
    (:length (:serial 2) (:parallel 2))  
 ) 

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

( DEFINE ( PROBLEM D1S1-8 )
    (:domain
    D1S1)
    (:init
        ( I5 ) 
        ( I4 ) 
        ( I8 ) 
        ( I3 ) 
        ( I1 ) 
        ( I2 ) 
        ( I7 ) 
        ( I6 ) 
     ) 
    (:goal
    ( AND
        ( G5 ) 
        ( G1 ) 
        ( G7 ) 
        ( G6 ) 
        ( G8 ) 
        ( G2 ) 
        ( G3 ) 
        ( G4 ) 
	) )
    (:length (:serial 8) (:parallel 8))
 ) 

( DEFINE ( PROBLEM D1S1-9 )
    (:domain
    D1S1)
    (:init
        ( I3 ) 
        ( I9 ) 
        ( I5 ) 
        ( I4 ) 
        ( I7 ) 
        ( I8 ) 
        ( I6 ) 
        ( I2 ) 
        ( I1 ) 
     ) 
    (:goal
    ( AND
        ( G1 ) 
        ( G5 ) 
        ( G4 ) 
        ( G6 ) 
        ( G3 ) 
        ( G8 ) 
        ( G9 ) 
        ( G7 ) 
        ( G2 ) 
	) )
    (:length (:serial 9) (:parallel 9)) 
 ) 

( DEFINE ( PROBLEM D1S1-10 )
    (:domain
    D1S1)
    (:init
        ( I1 ) 
        ( I3 ) 
        ( I7 ) 
        ( I4 ) 
        ( I2 ) 
        ( I10 ) 
        ( I6 ) 
        ( I8 ) 
        ( I9 ) 
        ( I5 ) 
     ) 
    (:goal
    ( AND
        ( G7 ) 
        ( G5 ) 
        ( G2 ) 
        ( G4 ) 
        ( G10 ) 
        ( G3 ) 
        ( G8 ) 
        ( G1 ) 
        ( G6 ) 
        ( G9 ) 
	) )
    (:length (:serial 10) (:parallel 10))
 ) 

