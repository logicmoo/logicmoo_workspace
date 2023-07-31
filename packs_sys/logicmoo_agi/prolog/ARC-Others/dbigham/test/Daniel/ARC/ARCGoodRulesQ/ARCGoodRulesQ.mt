(*
    Tests for: Daniel`ARC`ARCGoodRulesQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGoodRulesQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCGoodRulesQ[{}, {1, 2, 3}]
    ,
    False
    ,
    TestID -> "ARCGoodRulesQ-20220722-55M2SD"
]