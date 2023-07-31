(*
    Tests for: Daniel`ARC`ARCColorCountSameQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCColorCountSameQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCColorCountSameQ[{{1}}, {{1}}]
    ,
    True
    ,
    TestID -> "ARCColorCountSameQ-20221029-64AW2N"
]

Test[
    Daniel`ARC`ARCColorCountSameQ[{{1, -1, -1}}, {{1, -1}}]
    ,
    True
    ,
    TestID -> "ARCColorCountSameQ-20221029-A4SC3C"
]

Test[
    Daniel`ARC`ARCColorCountSameQ[{{1, -1, -1}}, {{1, -1}}, "IgnoreBackground" -> False]
    ,
    False
    ,
    TestID -> "ARCColorCountSameQ-20221029-P6NJYI"
]