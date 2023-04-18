(*
    Tests for: Daniel`ARC`ARCConclusionsSoFarMatchQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConclusionsSoFarMatchQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCConclusionsSoFarMatchQ[<|"MyProperty" -> 1|>, "MyProperty", 1]
    ,
    True
    ,
    TestID -> "ARCConclusionsSoFarMatchQ-20221023-KN2PNN"
]

Test[
    Daniel`ARC`ARCConclusionsSoFarMatchQ[<|"MyProperty" -> 1|>, "MyProperty", 1 | 2]
    ,
    True
    ,
    TestID -> "ARCConclusionsSoFarMatchQ-20221023-B1BYCU"
]

Test[
    Daniel`ARC`ARCConclusionsSoFarMatchQ[
        <|"MyProperty.InputValues" -> {1, 2}|>,
        "MyProperty",
        1 | 2
    ]
    ,
    True
    ,
    TestID -> "ARCConclusionsSoFarMatchQ-20221023-JSV0NY"
]

Test[
    Daniel`ARC`ARCConclusionsSoFarMatchQ[
        <|"MyProperty.InputValues" -> {1, 2, 3}|>,
        "MyProperty",
        1 | 2
    ]
    ,
    False
    ,
    TestID -> "ARCConclusionsSoFarMatchQ-20221023-NCU5QI"
]