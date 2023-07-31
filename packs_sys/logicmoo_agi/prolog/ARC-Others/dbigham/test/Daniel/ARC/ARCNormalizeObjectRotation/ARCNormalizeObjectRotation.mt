(*
    Tests for: Daniel`ARC`ARCNormalizeObjectRotation
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCNormalizeObjectRotation]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCNormalizeObjectRotation[
                    Utility`Sett[
                        Daniel`ARC`ARCImageRegionToObject[
                            <|
                                "Color" -> 2,
                                "Position" -> {1, 1},
                                "Image" -> {{2, 2, 2}, {2, -1, -1}, {2, -1, -1}},
                                "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}}
                            |>,
                            10,
                            10
                        ],
                        {"ParentWidth" -> 10, "ParentHeight" -> 10}
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{2, -1, -1}, {2, -1, -1}, {2, 2, 2}}],
        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}},
        "Shape" -> <|"Name" -> "L"|>,
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "L"|>
        },
        "Colors" -> {2},
        "Color" -> 2,
        "Position" -> {8, 1},
        "ColorCount" -> 1,
        "Y" -> 8,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 10,
        "X2" -> 3,
        "Y2Inverse" -> 8,
        "X2Inverse" -> 8,
        "ZOrder" -> 0,
        "YMiddle" -> 2,
        "XMiddle" -> 2,
        "Width" -> 3,
        "Height" -> 3,
        "Length" -> 3,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 9,
        "FilledArea" -> 5,
        "FilledProportion" -> 0.5555555555555556,
        "SurfacePixelCount" -> 5,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0,
        "ParentWidth" -> 10,
        "ParentHeight" -> 10
    |>
    ,
    TestID -> "ARCNormalizeObjectRotation-20220816-BXQJCC"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNormalizeObjectRotation[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
                "Shape" -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
                "Color" -> 1,
                "Colors" -> {1},
                "Position" -> {2, 3},
                "Y" -> 2,
                "X" -> 3,
                "Y2" -> 2,
                "X2" -> 5,
                "Width" -> 3,
                "Height" -> 1,
                "ParentWidth" -> 10,
                "ParentHeight" -> 10,
                "Transform" -> <|
                    "Type" -> "AddComponents",
                    "Components" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
                            "Shapes" -> {<|"Name" -> "Line"|>},
                            "Position" -> {2, 3},
                            "Y" -> 2,
                            "X" -> 3,
                            "YRelative" -> 1,
                            "XRelative" -> 1,
                            "Y2" -> 2,
                            "X2" -> 5,
                            "Width" -> 3,
                            "Height" -> 1
                        |>
                    }
                |>
            |>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}, {1}, {1}}],
        "Color" -> 1,
        "Colors" -> {1},
        "Position" -> {6, 2},
        "Y" -> 6,
        "X" -> 2,
        "Y2" -> 8,
        "X2" -> 2,
        "Width" -> 1,
        "Height" -> 3,
        "ParentWidth" -> 10,
        "ParentHeight" -> 10,
        "Transform" -> <|
            "Type" -> "AddComponents",
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}, {1}, {1}}],
                    "Shapes" -> {
                        <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}]|>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {{10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}}
                            ],
                            "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10},
                                    {10, 10, 10}
                                }
                            ],
                            "Transform" -> <|
                                "Type" -> "Scaled",
                                "Factor" -> 0.3333333333333333
                            |>
                        |>,
                        <|"Name" -> "Line"|>,
                        <|"Name" -> "Rectangle"|>,
                        <|"Name" -> "Line", "Angle" -> 90|>,
                        <|"Name" -> "Rectangle", "Filled" -> True|>
                    },
                    "Position" -> {6, 2},
                    "Y" -> 6,
                    "X" -> 2,
                    "YRelative" -> 1,
                    "XRelative" -> 1,
                    "Y2" -> 8,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 3
                |>
            }
        |>
    |>
    ,
    TestID -> "ARCNormalizeObjectRotation-20220816-2RT4XI"
]