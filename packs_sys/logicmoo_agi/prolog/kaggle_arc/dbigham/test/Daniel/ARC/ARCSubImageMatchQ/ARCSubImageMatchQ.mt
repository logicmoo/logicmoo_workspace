(*
    Tests for: Daniel`ARC`ARCSubImageMatchQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubImageMatchQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSubImageMatchQ[{{1, 2}, {4, 5}}, 1, 2, 2, 3, {{2, 3}, {5, 6}}]
    ,
    True
    ,
    TestID -> "ARCSubImageMatchQ-20221008-DCXLB0"
]

Test[
    Daniel`ARC`ARCSubImageMatchQ[{{1, 2}, {3, 4}}, 1, 1, 2, 2, {{1, 2}, {3, 4}}]
    ,
    True
    ,
    TestID -> "ARCSubImageMatchQ-20221008-EUCEI7"
]