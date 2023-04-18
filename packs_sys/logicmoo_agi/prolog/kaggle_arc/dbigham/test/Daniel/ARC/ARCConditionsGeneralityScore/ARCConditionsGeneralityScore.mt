(*
    Tests for: Daniel`ARC`ARCConditionsGeneralityScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConditionsGeneralityScore]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsGeneralityScore[<|"Shapes" -> <|"Name" -> "Square"|>|>]
    ]
    ,
    1.1
    ,
    TestID -> "ARCConditionsGeneralityScore-20220806-D4XXCF"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsGeneralityScore[<|"Shapes" -> <|"Name" -> "Rectangle"|>|>]
    ]
    ,
    1.
    ,
    TestID -> "ARCConditionsGeneralityScore-20220806-I34FGZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsGeneralityScore[
            <|"Shapes" -> <|"Name" -> "L"|>, "Colors" -> {1}|>
        ]
    ]
    ,
    2.
    ,
    TestID -> "ARCConditionsGeneralityScore-20220806-1W4K32"
]

Test[
    Daniel`ARC`ARCConditionsGeneralityScore[
        <|
            "Shapes" -> <|
                "Name" -> "L",
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>
        |>
    ]
    ,
    1.5
    ,
    TestID -> "ARCConditionsGeneralityScore-20220806-NO4EMM"
]