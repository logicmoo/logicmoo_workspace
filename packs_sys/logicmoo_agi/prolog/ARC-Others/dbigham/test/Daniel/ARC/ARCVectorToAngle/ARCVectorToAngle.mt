(*
    Tests for: Daniel`ARC`ARCVectorToAngle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCVectorToAngle]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCVectorToAngle[{-1, 0}]
    ,
    -90
    ,
    TestID -> "ARCVectorToAngle-20221110-DPB5RP"
]

Test[
    Daniel`ARC`ARCVectorToAngle[{-1, 1}]
    ,
    -45
    ,
    TestID -> "ARCVectorToAngle-20221110-UZVH3Q"
]

Test[
    Daniel`ARC`ARCVectorToAngle[{1, 1}]
    ,
    45
    ,
    TestID -> "ARCVectorToAngle-20221110-CGS0PE"
]