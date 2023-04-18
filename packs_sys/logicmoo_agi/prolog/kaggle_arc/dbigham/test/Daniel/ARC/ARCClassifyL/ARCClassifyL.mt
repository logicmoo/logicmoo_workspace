(*
    Tests for: Daniel`ARC`ARCClassifyL
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCClassifyL]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyL[{{1, -1, -1}, {1, -1, -1}, {1, 1, 1}}]
    ]
    ,
    <|"Name" -> "L"|>
    ,
    TestID -> "ARCClassifyL-20220717-C0CDCJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        ToString[Daniel`ARC`ARCClassifyL[{{1}}]]
    ]
    ,
    "Nothing"
    ,
    TestID -> "ARCClassifyL-20220717-8O7XSJ"
]