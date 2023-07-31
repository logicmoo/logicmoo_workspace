(*
    Tests for: Daniel`ARC`ARCToNegativeAngle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCToNegativeAngle]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCToNegativeAngle[90]
    ,
    -270
    ,
    TestID -> "ARCToNegativeAngle-20220816-AFUJB8"
]

Test[
    Daniel`ARC`ARCToNegativeAngle[180]
    ,
    -180
    ,
    TestID -> "ARCToNegativeAngle-20220816-PQKX9M"
]

Test[
    Daniel`ARC`ARCToNegativeAngle[270]
    ,
    -90
    ,
    TestID -> "ARCToNegativeAngle-20220816-GEEQQ9"
]

Test[
    Daniel`ARC`ARCToNegativeAngle[0]
    ,
    0
    ,
    TestID -> "ARCToNegativeAngle-20220816-WE6ZWY"
]

Test[
    Daniel`ARC`ARCToNegativeAngle[-360]
    ,
    0
    ,
    TestID -> "ARCToNegativeAngle-20220816-AK8IVD"
]

Test[
    Daniel`ARC`ARCToNegativeAngle[-450]
    ,
    -90
    ,
    TestID -> "ARCToNegativeAngle-20220816-08CO0R"
]