(*
    Tests for: Daniel`ARC`ARCLineTurnCount
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCLineTurnCount]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCLineTurnCount[{{1, 1}, {1, 2}, {1, 3}, {2, 3}, {3, 3}}]
    ,
    1
    ,
    TestID -> "ARCLineTurnCount-20220914-BIFC68"
]

Test[
    Daniel`ARC`ARCLineTurnCount[{{1, 1}, {1, 2}, {1, 3}}]
    ,
    0
    ,
    TestID -> "ARCLineTurnCount-20220914-F18WVS"
]

Test[
    Daniel`ARC`ARCLineTurnCount[{{1, 1}, {2, 2}, {3, 3}}]
    ,
    0
    ,
    TestID -> "ARCLineTurnCount-20220914-HCCY3I"
]

Test[
    Daniel`ARC`ARCLineTurnCount[{{1, 1}, {1, 2}, {1, 3}, {2, 3}, {3, 3}, {4, 3}, {4, 2}}]
    ,
    2
    ,
    TestID -> "ARCLineTurnCount-20220914-AHXZ5A"
]