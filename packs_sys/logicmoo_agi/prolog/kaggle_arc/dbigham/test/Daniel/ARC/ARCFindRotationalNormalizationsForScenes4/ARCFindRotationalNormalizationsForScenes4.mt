(*
    Tests for: Daniel`ARC`ARCFindRotationalNormalizationsForScenes4
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRotationalNormalizationsForScenes4]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFindRotationalNormalizationsForScenes4[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["5168d44c"]["Train"],
            "FormMultiColorCompositeObjects" -> False
        ]
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
            "RotationNormalization" -> <|"ObjectsAngle" -> 0|>
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
    TestID -> "ARCFindRotationalNormalizationsForScenes4-20221110-58JG13"
]