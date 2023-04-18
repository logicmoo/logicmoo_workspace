(*
    Tests for: Daniel`ARC`ARCVerticalOverlapQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCVerticalOverlapQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCVerticalOverlapQ[
            <|"Y" -> 1, "X" -> 1, "Width" -> 1, "Height" -> 2|>,
            <|"Y" -> 2, "X" -> 3, "Width" -> 1, "Height" -> 2|>
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCVerticalOverlapQ-20220804-KRWZBM"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCVerticalOverlapQ[
            <|"Y" -> 1, "X" -> 1, "Width" -> 1, "Height" -> 2|>,
            <|"Y" -> 4, "X" -> 3, "Width" -> 1, "Height" -> 2|>
        ]
    ]
    ,
    False
    ,
    TestID -> "ARCVerticalOverlapQ-20220804-QN9BKD"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCVerticalOverlapQ[
            <|"Y" -> 1, "X" -> 1, "Width" -> 1, "Height" -> 2|>,
            <|"Y" -> -1, "X" -> 3, "Width" -> 1, "Height" -> 2|>
        ]
    ]
    ,
    False
    ,
    TestID -> "ARCVerticalOverlapQ-20220804-GW4SV8"
]