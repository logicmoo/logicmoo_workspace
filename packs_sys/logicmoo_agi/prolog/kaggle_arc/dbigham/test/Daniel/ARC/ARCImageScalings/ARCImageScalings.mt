(*
    Tests for: Daniel`ARC`ARCImageScalings
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageScalings]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCImageScalings[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, -1, -1, 1, 1},
                    {1, 1, -1, -1, 1, 1},
                    {-1, -1, 1, 1, -1, -1},
                    {-1, -1, 1, 1, -1, -1},
                    {-1, -1, -1, -1, 1, 1},
                    {-1, -1, -1, -1, 1, 1}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, -1, -1, -1, 1, 1, 1},
                    {1, 1, 1, -1, -1, -1, 1, 1, 1},
                    {1, 1, 1, -1, -1, -1, 1, 1, 1},
                    {-1, -1, -1, 1, 1, 1, -1, -1, -1},
                    {-1, -1, -1, 1, 1, 1, -1, -1, -1},
                    {-1, -1, -1, 1, 1, 1, -1, -1, -1},
                    {-1, -1, -1, -1, -1, -1, 1, 1, 1},
                    {-1, -1, -1, -1, -1, -1, 1, 1, 1},
                    {-1, -1, -1, -1, -1, -1, 1, 1, 1}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>
    }
    ,
    TestID -> "ARCImageScalings-20220903-Z90IMK"
]

Test[
    Daniel`ARC`ARCImageScalings[{{1, 1}, {1, 1}}]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 2.|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{1, 1, 1, 1}, {1, 1, 1, 1}, {1, 1, 1, 1}, {1, 1, 1, 1}}
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>
    }
    ,
    TestID -> "ARCImageScalings-20220903-0BU4F1"
]

Test[
    Daniel`ARC`ARCImageScalings[{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}][[1]]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}}],
        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 3.|>
    |>
    ,
    TestID -> "ARCImageScalings-20220904-OV6BCS"
]

Test[
    Daniel`ARC`ARCImageScalings[{{1}}, "IncludeNoopTransforms" -> True]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 1.|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>
    }
    ,
    TestID -> "ARCImageScalings-20220908-41566E"
]