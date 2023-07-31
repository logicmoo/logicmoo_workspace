(*
    Tests for: Daniel`ARC`ARCAngleForTwoPoints
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCAngleForTwoPoints]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {5, 6}]
    ,
    0
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-3FBMS4"
]

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {1, 5}]
    ,
    90
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-K0W2WD"
]

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {5, 1}]
    ,
    0
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-LYK4HO"
]

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {10, 5}]
    ,
    90
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-N4NCMS"
]

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {1, 9}]
    ,
    45
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-BC3CXC"
]

Test[
    Daniel`ARC`ARCAngleForTwoPoints[{5, 5}, {1, 1}]
    ,
    135
    ,
    TestID -> "ARCAngleForTwoPoints-20220827-9ULCB6"
]