(*
    Tests for: Daniel`ARC`ARCCombineAlternatives
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCombineAlternatives]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCCombineAlternatives[1 | (1 | 2) | 2 | 3 | {9, 9}]
    ,
    1 | 2 | 3 | {9, 9}
    ,
    TestID -> "ARCCombineAlternatives-20220819-T7ZENE"
]