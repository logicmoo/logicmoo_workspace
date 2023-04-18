(*
    Tests for: Daniel`ARC`ARCImageShapes
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageShapes]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[Daniel`ARC`ARCImageShapes[{{1}}]]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}}
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
        |>
    }
    ,
    TestID -> "ARCImageShapes-20220908-VQZ32Q"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageShapes[{{1, 2}}]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{1, 2}}]|>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}, {2}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2, 1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2}, {1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2, 1}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 2, 2}, {1, 1, 2, 2}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{1, 1, 1, 2, 2, 2}, {1, 1, 1, 2, 2, 2}, {1, 1, 1, 2, 2, 2}}
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, 1, 2, 2, 2, 2},
                    {1, 1, 1, 1, 2, 2, 2, 2},
                    {1, 1, 1, 1, 2, 2, 2, 2},
                    {1, 1, 1, 1, 2, 2, 2, 2}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, 1, 1, 2, 2, 2, 2, 2},
                    {1, 1, 1, 1, 1, 2, 2, 2, 2, 2},
                    {1, 1, 1, 1, 1, 2, 2, 2, 2, 2},
                    {1, 1, 1, 1, 1, 2, 2, 2, 2, 2},
                    {1, 1, 1, 1, 1, 2, 2, 2, 2, 2}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
        |>
    }
    ,
    TestID -> "ARCImageShapes-20220908-22VDDH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageShapes[{{1}}, "IncludeNoopTransforms" -> True]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 1.|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}}
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
        |>
    }
    ,
    TestID -> "ARCImageShapes-20220908-68H36B"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageShapes[{{1}}, "Monochrome" -> {{10}}]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}, {10, 10, 10, 10}}
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10},
                    {10, 10, 10, 10, 10}
                }
            ],
            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
        |>
    }
    ,
    TestID -> "ARCImageShapes-20220908-C0M9PS"
]