(*
    Tests for: Daniel`ARC`ARCCheckForSceneRepairUsingImputationQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCheckForSceneRepairUsingImputationQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCCheckForSceneRepairUsingImputationQ[
        Daniel`ARC`ARCParseFile["3631a71a"]["Train"]
    ]
    ,
    True
    ,
    TestID -> "ARCCheckForSceneRepairUsingImputationQ-20221002-ALYPRL"
]