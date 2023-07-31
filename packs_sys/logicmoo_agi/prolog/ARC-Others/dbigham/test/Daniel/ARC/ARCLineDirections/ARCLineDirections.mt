(*
    Tests for: Daniel`ARC`ARCLineDirections
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCLineDirections]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCLineDirections[
            <|"Shape" -> <|"Name" -> "Line"|>, "Width" -> 3, "Height" -> 1|>
        ]
    ]
    ,
    {{0, -1}, {0, 1}}
    ,
    TestID -> "ARCLineDirections-20220826-X6ZW7F"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCLineDirections[
            <|"Shape" -> <|"Name" -> "Line"|>, "Width" -> 1, "Height" -> 3|>
        ]
    ]
    ,
    {{-1, 0}, {1, 0}}
    ,
    TestID -> "ARCLineDirections-20220826-PT30RE"
]