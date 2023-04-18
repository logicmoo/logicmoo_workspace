(*
    Tests for: Daniel`ARC`ARCSymmetryLocations
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSymmetryLocations]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSymmetryLocations[10, 10, {1, 1}]
    ,
    {{10, 1}, {1, 10}, {10, 10}}
    ,
    TestID -> "ARCSymmetryLocations-20220928-DXLR5D"
]

Test[
    Daniel`ARC`ARCSymmetryLocations[10, 10, {2, 2}]
    ,
    {{9, 2}, {2, 9}, {9, 9}}
    ,
    TestID -> "ARCSymmetryLocations-20220928-L7P0G2"
]