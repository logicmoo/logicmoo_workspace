(*
    Tests for: Daniel`ARC`ARCConclusionsInvolveRotation
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConclusionsInvolveRotation]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCConclusionsInvolveRotation[
        {
            <|
                "Input" -> <|
                    "Shape" -> <|
                        "Name" -> "Triangle",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                |>
            |>,
            <|"Input" -> <|"Shape" -> <|"Name" -> "Triangle"|>|>|>
        }
    ]
    ,
    True
    ,
    TestID -> "ARCConclusionsInvolveRotation-20220821-9EBEAX"
]

Test[
    Daniel`ARC`ARCConclusionsInvolveRotation[
        {
            <|
                "Input" -> <|
                    "Shape" -> <|
                        "Name" -> "Triangle",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                |>
            |>,
            <|
                "Input" -> <|
                    "Shape" -> <|
                        "Name" -> "Triangle",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                |>
            |>
        }
    ]
    ,
    False
    ,
    TestID -> "ARCConclusionsInvolveRotation-20220821-3OOHYX"
]

Test[
    Daniel`ARC`ARCConclusionsInvolveRotation[
        {
            <|"Input" -> <|"Shape" -> <|"Name" -> "Triangle"|>|>|>,
            <|"Input" -> <|"Shape" -> <|"Name" -> "Triangle"|>|>|>
        }
    ]
    ,
    False
    ,
    TestID -> "ARCConclusionsInvolveRotation-20220821-9SILFZ"
]

Test[
    Daniel`ARC`ARCConclusionsInvolveRotation[
        {
            <|
                "Input" -> <|
                    "Shape" -> <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                |>
            |>,
            <|"Input" -> <|"Shape" -> <|"Name" -> "Triangle"|>|>|>
        }
    ]
    ,
    False
    ,
    TestID -> "ARCConclusionsInvolveRotation-20220821-O42HMR"
]