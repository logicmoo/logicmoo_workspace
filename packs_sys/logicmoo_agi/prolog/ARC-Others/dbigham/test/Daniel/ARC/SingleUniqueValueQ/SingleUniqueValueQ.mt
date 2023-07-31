(*
    Tests for: Daniel`ARC`SingleUniqueValueQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`SingleUniqueValueQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SingleUniqueValueQ[{1, 1, 1}]
    ,
    True
    ,
    TestID -> "SingleUniqueValueQ-20221004-86P4CC"
]

Test[
    Daniel`ARC`SingleUniqueValueQ[{1, 2, 3}]
    ,
    False
    ,
    TestID -> "SingleUniqueValueQ-20221004-G8EGN2"
]

Test[
    Daniel`ARC`SingleUniqueValueQ[{}]
    ,
    False
    ,
    TestID -> "SingleUniqueValueQ-20221004-DY1A4E"
]