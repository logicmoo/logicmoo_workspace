(*
    Tests for: Daniel`ARC`ARCFindRotationalNormalizationsForScenes3
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRotationalNormalizationsForScenes3]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFindRotationalNormalizationsForScenes3[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["5168d44c"]["Train"],
            "FormMultiColorCompositeObjects" -> False
        ],
        "FavoredRotationAngle" -> -90
    ]
    ,
    {
        <|
            "Input" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {2, 3, 2, 3, 0, 3, 0, 3, 0, 3, 0, 3, 0},
                    {2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "Output" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 3, 2, 3, 2, 3, 0, 3, 0, 3, 0, 3, 0},
                    {0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "RotationNormalization" -> <|"ObjectsAngle" -> 0, "FavoredRotationAngle" -> -90|>
        |>,
        <|
            "Input" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0},
                    {3, 0, 3, 2, 3, 2, 3, 0, 3, 0, 3, 0, 3},
                    {0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "Output" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0},
                    {3, 0, 3, 0, 3, 2, 3, 2, 3, 0, 3, 0, 3},
                    {0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "NormalizationAngle" -> -90
        |>,
        <|
            "Input" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 2, 2, 2, 0, 0, 0},
                    {3, 2, 3, 2, 3, 0, 3},
                    {0, 2, 2, 2, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "Output" -> Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 2, 2, 2, 0},
                    {3, 0, 3, 2, 3, 2, 3},
                    {0, 0, 0, 2, 2, 2, 0},
                    {0, 0, 0, 0, 0, 0, 0}
                }
            ],
            "NormalizationAngle" -> -90
        |>
    }
    ,
    TestID -> "ARCFindRotationalNormalizationsForScenes3-20221110-4RURPA"
]