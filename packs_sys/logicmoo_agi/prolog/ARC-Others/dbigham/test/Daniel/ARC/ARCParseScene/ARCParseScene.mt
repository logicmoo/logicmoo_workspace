(*
    Tests for: Daniel`ARC`ARCParseScene
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCParseScene]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["9565186b"]["Train", 1, "Input"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 3,
        "Height" -> 3,
        "Scene" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}],
        "Objects" -> {
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}],
                "PixelPositions" -> {
                    {1, 1},
                    {1, 2},
                    {1, 3},
                    {2, 1},
                    {2, 2},
                    {2, 3},
                    {3, 1},
                    {3, 2},
                    {3, 3}
                },
                "Shape" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}],
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {8, 1, 2}, {8, 8, 2}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{8, 8, 2}, {8, 1, 2}, {2, 2, 2}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2, 8, 8}, {2, 1, 8}, {2, 2, 2}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2, 8, 8}, {2, 1, 8}, {2, 2, 2}}],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {8, 1, 2}, {8, 8, 2}}],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2},
                                {2, 2, 1, 1, 8, 8},
                                {2, 2, 1, 1, 8, 8},
                                {2, 2, 8, 8, 8, 8},
                                {2, 2, 8, 8, 8, 8}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 1, 1, 1, 8, 8, 8},
                                {2, 2, 2, 1, 1, 1, 8, 8, 8},
                                {2, 2, 2, 1, 1, 1, 8, 8, 8},
                                {2, 2, 2, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 8, 8, 8, 8, 8, 8}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 1, 1, 1, 1, 8, 8, 8, 8},
                                {2, 2, 2, 2, 1, 1, 1, 1, 8, 8, 8, 8},
                                {2, 2, 2, 2, 1, 1, 1, 1, 8, 8, 8, 8},
                                {2, 2, 2, 2, 1, 1, 1, 1, 8, 8, 8, 8},
                                {2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
                                {2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
                                {2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Square", "Filled" -> True|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {1, 2, 8},
                "Position" -> {1, 1},
                "MonochromeImage" -> Daniel`ARC`ARCScene[
                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                ],
                "ColorCount" -> 3,
                "MostUsedColor" -> 2,
                "Y" -> 1,
                "X" -> 1,
                "YInverse" -> 3,
                "XInverse" -> 3,
                "Y2" -> 3,
                "X2" -> 3,
                "Y2Inverse" -> 1,
                "X2Inverse" -> 1,
                "ZOrder" -> 0,
                "YMiddle" -> 2,
                "XMiddle" -> 2,
                "Width" -> 3,
                "Height" -> 3,
                "Length" -> 3,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 9,
                "FilledArea" -> 9,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 8,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "Components" -> {
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}],
                        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}},
                        "Shape" -> <|
                            "Name" -> "L",
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                        |>,
                        "Shapes" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}
                                ]
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}
                                ],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}
                                ],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, -1, -1},
                                        {10, 10, -1, -1, -1, -1},
                                        {10, 10, -1, -1, -1, -1},
                                        {10, 10, -1, -1, -1, -1}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, -1, -1, -1}
                                    }
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        }
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                            |>,
                            <|"Name" -> "L"|>,
                            <|
                                "Name" -> "L",
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                            |>,
                            <|
                                "Name" -> "L",
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            |>
                        },
                        "Colors" -> {2},
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}
                        ],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 2,
                        "Y" -> 1,
                        "X" -> 1,
                        "YInverse" -> 3,
                        "XInverse" -> 3,
                        "Y2" -> 3,
                        "X2" -> 3,
                        "Y2Inverse" -> 1,
                        "X2Inverse" -> 1,
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
                        "YRelative" -> 0,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{1}}],
                        "PixelPositions" -> {{2, 2}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {1},
                        "Color" -> 1,
                        "Position" -> {2, 2},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 1,
                        "Y" -> 2,
                        "X" -> 2,
                        "YInverse" -> 2,
                        "XInverse" -> 2,
                        "Y2" -> 2,
                        "X2" -> 2,
                        "Y2Inverse" -> 2,
                        "X2Inverse" -> 2,
                        "ZOrder" -> 0,
                        "YMiddle" -> 2,
                        "XMiddle" -> 2,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 1,
                        "XRelative" -> 1
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 8}, {8, 8}}],
                        "PixelPositions" -> {{2, 3}, {3, 2}, {3, 3}},
                        "Shape" -> <|
                            "Name" -> "L",
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                        |>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, -1}}],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}}],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}}],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, 10, 10},
                                        {-1, -1, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, -1, 10, 10, 10},
                                        {-1, -1, -1, 10, 10, 10},
                                        {-1, -1, -1, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, -1, -1, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, -1, -1, -1, 10, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, -1, 10, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, -1, 10, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, -1, 10, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, -1, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                            |>,
                            <|"Name" -> "L"|>,
                            <|
                                "Name" -> "L",
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                            |>,
                            <|
                                "Name" -> "L",
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                            |>
                        },
                        "Colors" -> {8},
                        "Color" -> 8,
                        "Position" -> {2, 2},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 8,
                        "Y" -> 2,
                        "X" -> 2,
                        "YInverse" -> 2,
                        "XInverse" -> 2,
                        "Y2" -> 3,
                        "X2" -> 3,
                        "Y2Inverse" -> 1,
                        "X2Inverse" -> 1,
                        "ZOrder" -> 0,
                        "Width" -> 2,
                        "Height" -> 2,
                        "Length" -> 2,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 4,
                        "FilledArea" -> 3,
                        "FilledProportion" -> 0.75,
                        "SurfacePixelCount" -> 3,
                        "VerticalLineSymmetry" -> False,
                        "HorizontalLineSymmetry" -> False,
                        "VerticalAndHorizontalLineSymmetry" -> False,
                        "HollowCount" -> 0,
                        "YRelative" -> 1,
                        "XRelative" -> 1
                    |>
                },
                "SecondMostUsedColor" -> 8,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 1,
                "X.InverseRank" -> 1,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 1,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 1,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 1,
                "Y2Inverse.Rank" -> 1,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 1,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 1,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 1,
                "Area.InverseRank" -> 1,
                "FilledArea.Rank" -> 1,
                "FilledArea.InverseRank" -> 1,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 1,
                "SurfacePixelCount.Rank" -> 1,
                "SurfacePixelCount.InverseRank" -> 1,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 1,
                "MostUsedColor.InverseRank" -> 1,
                "SecondMostUsedColor.Rank" -> 1,
                "SecondMostUsedColor.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>
        },
        "ObjectCount" -> 1,
        "Colors" -> {1, 2, 8},
        "ColorCount" -> 3,
        "MostUsedColor" -> 2,
        "SecondMostUsedColor" -> 8,
        "YMiddle" -> 2,
        "XMiddle" -> 2,
        "Length" -> 3,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 9,
        "FilledArea" -> 9,
        "FilledProportion" -> 1.,
        "SurfacePixelCount" -> 8,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCParseScene-20220717-RTEVHR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["0ca9ddb6"]["Train", 1, "Output"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 9,
        "Height" -> 9,
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 4, 0, 4, 0, 0, 0, 0, 0},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {0, 4, 0, 4, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 7, 0, 0},
                {0, 0, 0, 0, 0, 7, 1, 7, 0},
                {0, 0, 0, 0, 0, 0, 7, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0}
            }
        ],
        "Objects" -> {
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
                "PixelPositions" -> {{3, 2}, {3, 4}, {4, 3}, {5, 2}, {5, 4}},
                "Shape" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {4, 4, -1, -1, 4, 4},
                                {4, 4, -1, -1, 4, 4},
                                {-1, -1, 2, 2, -1, -1},
                                {-1, -1, 2, 2, -1, -1},
                                {4, 4, -1, -1, 4, 4},
                                {4, 4, -1, -1, 4, 4}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {4, 4, 4, -1, -1, -1, 4, 4, 4},
                                {4, 4, 4, -1, -1, -1, 4, 4, 4},
                                {4, 4, 4, -1, -1, -1, 4, 4, 4},
                                {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                                {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                                {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                                {4, 4, 4, -1, -1, -1, 4, 4, 4},
                                {4, 4, 4, -1, -1, -1, 4, 4, 4},
                                {4, 4, 4, -1, -1, -1, 4, 4, 4}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {-1, -1, -1, -1, 2, 2, 2, 2, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 2, 2, 2, 2, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 2, 2, 2, 2, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 2, 2, 2, 2, -1, -1, -1, -1},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4},
                                {4, 4, 4, 4, -1, -1, -1, -1, 4, 4, 4, 4}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {-1, -1, -1, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4},
                                {4, 4, 4, 4, 4, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>
                },
                "Colors" -> {2, 4},
                "Position" -> {3, 2},
                "MonochromeImage" -> Daniel`ARC`ARCScene[
                    {{10, -1, 10}, {-1, 10, -1}, {10, -1, 10}}
                ],
                "ColorCount" -> 2,
                "MostUsedColor" -> 4,
                "Y" -> 3,
                "X" -> 2,
                "YInverse" -> 7,
                "XInverse" -> 8,
                "Y2" -> 5,
                "X2" -> 4,
                "Y2Inverse" -> 5,
                "X2Inverse" -> 6,
                "ZOrder" -> 0,
                "YMiddle" -> 4,
                "XMiddle" -> 3,
                "Width" -> 3,
                "Height" -> 3,
                "Length" -> 3,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 9,
                "FilledArea" -> 5,
                "FilledProportion" -> 0.5555555555555556,
                "SurfacePixelCount" -> 5,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "Components" -> {
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{3, 2}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {4},
                        "Color" -> 4,
                        "Position" -> {3, 2},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 4,
                        "Y" -> 3,
                        "X" -> 2,
                        "YInverse" -> 7,
                        "XInverse" -> 8,
                        "Y2" -> 3,
                        "X2" -> 2,
                        "Y2Inverse" -> 7,
                        "X2Inverse" -> 8,
                        "ZOrder" -> 0,
                        "YMiddle" -> 3,
                        "XMiddle" -> 2,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 0,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{3, 4}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {4},
                        "Color" -> 4,
                        "Position" -> {3, 4},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 4,
                        "Y" -> 3,
                        "X" -> 4,
                        "YInverse" -> 7,
                        "XInverse" -> 6,
                        "Y2" -> 3,
                        "X2" -> 4,
                        "Y2Inverse" -> 7,
                        "X2Inverse" -> 6,
                        "ZOrder" -> 0,
                        "YMiddle" -> 3,
                        "XMiddle" -> 4,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 0,
                        "XRelative" -> 2
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "PixelPositions" -> {{4, 3}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {2},
                        "Color" -> 2,
                        "Position" -> {4, 3},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 2,
                        "Y" -> 4,
                        "X" -> 3,
                        "YInverse" -> 6,
                        "XInverse" -> 7,
                        "Y2" -> 4,
                        "X2" -> 3,
                        "Y2Inverse" -> 6,
                        "X2Inverse" -> 7,
                        "ZOrder" -> 0,
                        "YMiddle" -> 4,
                        "XMiddle" -> 3,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 1,
                        "XRelative" -> 1
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{5, 2}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {4},
                        "Color" -> 4,
                        "Position" -> {5, 2},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 4,
                        "Y" -> 5,
                        "X" -> 2,
                        "YInverse" -> 5,
                        "XInverse" -> 8,
                        "Y2" -> 5,
                        "X2" -> 2,
                        "Y2Inverse" -> 5,
                        "X2Inverse" -> 8,
                        "ZOrder" -> 0,
                        "YMiddle" -> 5,
                        "XMiddle" -> 2,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 2,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{5, 4}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {4},
                        "Color" -> 4,
                        "Position" -> {5, 4},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 4,
                        "Y" -> 5,
                        "X" -> 4,
                        "YInverse" -> 5,
                        "XInverse" -> 6,
                        "Y2" -> 5,
                        "X2" -> 4,
                        "Y2Inverse" -> 5,
                        "X2Inverse" -> 6,
                        "ZOrder" -> 0,
                        "YMiddle" -> 5,
                        "XMiddle" -> 4,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 2,
                        "XRelative" -> 2
                    |>
                },
                "SecondMostUsedColor" -> 2,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 2,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 2,
                "X.InverseRank" -> 1,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 2,
                "XInverse.Rank" -> 1,
                "XInverse.InverseRank" -> 2,
                "Y2.Rank" -> 2,
                "Y2.InverseRank" -> 1,
                "X2.Rank" -> 2,
                "X2.InverseRank" -> 1,
                "Y2Inverse.Rank" -> 1,
                "Y2Inverse.InverseRank" -> 2,
                "X2Inverse.Rank" -> 1,
                "X2Inverse.InverseRank" -> 2,
                "YMiddle.Rank" -> 2,
                "YMiddle.InverseRank" -> 1,
                "XMiddle.Rank" -> 2,
                "XMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 1,
                "Area.InverseRank" -> 1,
                "FilledArea.Rank" -> 1,
                "FilledArea.InverseRank" -> 1,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 1,
                "SurfacePixelCount.Rank" -> 1,
                "SurfacePixelCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 2,
                "MostUsedColor.InverseRank" -> 1,
                "SecondMostUsedColor.Rank" -> 1,
                "SecondMostUsedColor.InverseRank" -> 2,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
                "PixelPositions" -> {{6, 7}, {7, 6}, {7, 7}, {7, 8}, {8, 7}},
                "Shape" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, 7, 7, -1, -1},
                                {-1, -1, 7, 7, -1, -1},
                                {7, 7, 1, 1, 7, 7},
                                {7, 7, 1, 1, 7, 7},
                                {-1, -1, 7, 7, -1, -1},
                                {-1, -1, 7, 7, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1},
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1},
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1},
                                {7, 7, 7, 1, 1, 1, 7, 7, 7},
                                {7, 7, 7, 1, 1, 1, 7, 7, 7},
                                {7, 7, 7, 1, 1, 1, 7, 7, 7},
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1},
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1},
                                {-1, -1, -1, 7, 7, 7, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {7, 7, 7, 7, 1, 1, 1, 1, 7, 7, 7, 7},
                                {7, 7, 7, 7, 1, 1, 1, 1, 7, 7, 7, 7},
                                {7, 7, 7, 7, 1, 1, 1, 1, 7, 7, 7, 7},
                                {7, 7, 7, 7, 1, 1, 1, 1, 7, 7, 7, 7},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1},
                                {-1, -1, -1, -1, 7, 7, 7, 7, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 7, 7, 7, 7},
                                {7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 7, 7, 7, 7},
                                {7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 7, 7, 7, 7},
                                {7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 7, 7, 7, 7},
                                {7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 7, 7, 7, 7},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1},
                                {-1, -1, -1, -1, -1, 7, 7, 7, 7, 7, -1, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>
                },
                "Colors" -> {1, 7},
                "Position" -> {6, 6},
                "MonochromeImage" -> Daniel`ARC`ARCScene[
                    {{-1, 10, -1}, {10, 10, 10}, {-1, 10, -1}}
                ],
                "ColorCount" -> 2,
                "MostUsedColor" -> 7,
                "Y" -> 6,
                "X" -> 6,
                "YInverse" -> 4,
                "XInverse" -> 4,
                "Y2" -> 8,
                "X2" -> 8,
                "Y2Inverse" -> 2,
                "X2Inverse" -> 2,
                "ZOrder" -> 0,
                "YMiddle" -> 7,
                "XMiddle" -> 7,
                "Width" -> 3,
                "Height" -> 3,
                "Length" -> 3,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 9,
                "FilledArea" -> 5,
                "FilledProportion" -> 0.5555555555555556,
                "SurfacePixelCount" -> 4,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "Components" -> {
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}],
                        "PixelPositions" -> {{6, 7}, {7, 6}, {7, 8}, {8, 7}},
                        "Shape" -> Daniel`ARC`ARCScene[
                            {{-1, 10, -1}, {10, -1, 10}, {-1, 10, -1}}
                        ],
                        "Shapes" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{-1, 10, -1}, {10, -1, 10}, {-1, 10, -1}}
                                ]
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, 10, 10, -1, -1},
                                        {-1, -1, 10, 10, -1, -1},
                                        {10, 10, -1, -1, 10, 10},
                                        {10, 10, -1, -1, 10, 10},
                                        {-1, -1, 10, 10, -1, -1},
                                        {-1, -1, 10, 10, -1, -1}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                                        {10, 10, 10, -1, -1, -1, 10, 10, 10},
                                        {10, 10, 10, -1, -1, -1, 10, 10, 10},
                                        {10, 10, 10, -1, -1, -1, 10, 10, 10},
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                                        {-1, -1, -1, 10, 10, 10, -1, -1, -1}
                                    }
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1},
                                        {-1, -1, -1, -1, 10, 10, 10, 10, -1, -1, -1, -1}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        },
                                        {
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1
                                        }
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                            |>
                        },
                        "Colors" -> {7},
                        "Color" -> 7,
                        "Position" -> {6, 6},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {{-1, 10, -1}, {10, -1, 10}, {-1, 10, -1}}
                        ],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 7,
                        "Y" -> 6,
                        "X" -> 6,
                        "YInverse" -> 4,
                        "XInverse" -> 4,
                        "Y2" -> 8,
                        "X2" -> 8,
                        "Y2Inverse" -> 2,
                        "X2Inverse" -> 2,
                        "ZOrder" -> 0,
                        "YMiddle" -> 7,
                        "XMiddle" -> 7,
                        "Width" -> 3,
                        "Height" -> 3,
                        "Length" -> 3,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 9,
                        "FilledArea" -> 4,
                        "FilledProportion" -> 0.4444444444444444,
                        "SurfacePixelCount" -> 4,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 0,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{1}}],
                        "PixelPositions" -> {{7, 7}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {1},
                        "Color" -> 1,
                        "Position" -> {7, 7},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 1,
                        "Y" -> 7,
                        "X" -> 7,
                        "YInverse" -> 3,
                        "XInverse" -> 3,
                        "Y2" -> 7,
                        "X2" -> 7,
                        "Y2Inverse" -> 3,
                        "X2Inverse" -> 3,
                        "ZOrder" -> 0,
                        "YMiddle" -> 7,
                        "XMiddle" -> 7,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 1,
                        "XRelative" -> 1
                    |>
                },
                "SecondMostUsedColor" -> 1,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 2,
                "X.Rank" -> 1,
                "X.InverseRank" -> 2,
                "YInverse.Rank" -> 2,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 2,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 2,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 2,
                "Y2Inverse.Rank" -> 2,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 2,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 2,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 2,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 1,
                "Area.InverseRank" -> 1,
                "FilledArea.Rank" -> 1,
                "FilledArea.InverseRank" -> 1,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 1,
                "SurfacePixelCount.Rank" -> 2,
                "SurfacePixelCount.InverseRank" -> 1,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 1,
                "MostUsedColor.InverseRank" -> 2,
                "SecondMostUsedColor.Rank" -> 2,
                "SecondMostUsedColor.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>
        },
        "ObjectCount" -> 2,
        "Colors" -> {1, 2, 4, 7},
        "ColorCount" -> 4,
        "YMiddle" -> 5,
        "XMiddle" -> 5,
        "Length" -> 9,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 81,
        "FilledArea" -> 10,
        "FilledProportion" -> 0.12345679012345678,
        "SurfacePixelCount" -> 9,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCParseScene-20220718-W0VVO9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["228f6490"]["Train", 1, "Input"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 10,
        "Height" -> 10,
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {7, 0, 0, 0, 0, 0, 0, 0, 7, 7},
                {0, 5, 5, 5, 5, 5, 0, 0, 0, 0},
                {0, 5, 0, 0, 5, 5, 0, 6, 6, 0},
                {0, 5, 0, 0, 5, 5, 0, 0, 0, 0},
                {0, 5, 5, 5, 5, 5, 0, 0, 0, 0},
                {0, 5, 5, 5, 5, 5, 0, 0, 7, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 7, 5, 5, 5, 5, 5},
                {0, 8, 8, 0, 0, 5, 5, 0, 0, 5},
                {0, 8, 8, 0, 0, 5, 5, 5, 5, 5}
            }
        ],
        "Objects" -> {
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {7, -1, -1, -1, -1, -1},
                        {-1, 5, 5, 5, 5, 5},
                        {-1, 5, -1, -1, 5, 5},
                        {-1, 5, -1, -1, 5, 5},
                        {-1, 5, 5, 5, 5, 5},
                        {-1, 5, 5, 5, 5, 5}
                    }
                ],
                "PixelPositions" -> {
                    {1, 1},
                    {2, 2},
                    {2, 3},
                    {2, 4},
                    {2, 5},
                    {2, 6},
                    {3, 2},
                    {3, 5},
                    {3, 6},
                    {4, 2},
                    {4, 5},
                    {4, 6},
                    {5, 2},
                    {5, 3},
                    {5, 4},
                    {5, 5},
                    {5, 6},
                    {6, 2},
                    {6, 3},
                    {6, 4},
                    {6, 5},
                    {6, 6}
                },
                "Shape" -> Daniel`ARC`ARCScene[
                    {
                        {7, -1, -1, -1, -1, -1},
                        {-1, 5, 5, 5, 5, 5},
                        {-1, 5, -1, -1, 5, 5},
                        {-1, 5, -1, -1, 5, 5},
                        {-1, 5, 5, 5, 5, 5},
                        {-1, 5, 5, 5, 5, 5}
                    }
                ],
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {7, -1, -1, -1, -1, -1},
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, 5, 5, 5, 5}
                            }
                        ]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, -1, -1, 7},
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, 5, 5, 5, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, 5, 5, 5, -1},
                                {-1, -1, -1, -1, -1, 7}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, 5, 5, 5, 5},
                                {7, -1, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, 5, 5, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, -1, -1, 5, 5},
                                {-1, 5, 5, 5, 5, 5},
                                {7, -1, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, -1, -1, 7},
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, -1, -1, 5, -1},
                                {5, 5, 5, 5, 5, -1},
                                {5, 5, 5, 5, 5, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {7, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                                {7, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, -1, -1, -1, -1, 5, 5, 5, 5},
                                {-1, -1, 5, 5, -1, -1, -1, -1, 5, 5, 5, 5},
                                {-1, -1, 5, 5, -1, -1, -1, -1, 5, 5, 5, 5},
                                {-1, -1, 5, 5, -1, -1, -1, -1, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {
                                    7,
                                    7,
                                    7,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1
                                },
                                {
                                    7,
                                    7,
                                    7,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1
                                },
                                {
                                    7,
                                    7,
                                    7,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1
                                },
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5
                                },
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>
                },
                "Colors" -> {5, 7},
                "Position" -> {1, 1},
                "MonochromeImage" -> Daniel`ARC`ARCScene[
                    {
                        {10, -1, -1, -1, -1, -1},
                        {-1, 10, 10, 10, 10, 10},
                        {-1, 10, -1, -1, 10, 10},
                        {-1, 10, -1, -1, 10, 10},
                        {-1, 10, 10, 10, 10, 10},
                        {-1, 10, 10, 10, 10, 10}
                    }
                ],
                "ColorCount" -> 2,
                "MostUsedColor" -> 5,
                "Y" -> 1,
                "X" -> 1,
                "YInverse" -> 10,
                "XInverse" -> 10,
                "Y2" -> 6,
                "X2" -> 6,
                "Y2Inverse" -> 5,
                "X2Inverse" -> 5,
                "ZOrder" -> 0,
                "Width" -> 6,
                "Height" -> 6,
                "Length" -> 6,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 36,
                "FilledArea" -> 22,
                "FilledProportion" -> 0.6111111111111112,
                "SurfacePixelCount" -> 17,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 1,
                "Components" -> {
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{7}}],
                        "PixelPositions" -> {{1, 1}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {7},
                        "Color" -> 7,
                        "Position" -> {1, 1},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 7,
                        "Y" -> 1,
                        "X" -> 1,
                        "YInverse" -> 10,
                        "XInverse" -> 10,
                        "Y2" -> 1,
                        "X2" -> 1,
                        "Y2Inverse" -> 10,
                        "X2Inverse" -> 10,
                        "ZOrder" -> 0,
                        "YMiddle" -> 1,
                        "XMiddle" -> 1,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 0,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {5, 5, 5, 5, 5},
                                {5, -1, -1, 5, 5},
                                {5, -1, -1, 5, 5},
                                {5, 5, 5, 5, 5},
                                {5, 5, 5, 5, 5}
                            }
                        ],
                        "PixelPositions" -> {
                            {2, 2},
                            {2, 3},
                            {2, 4},
                            {2, 5},
                            {2, 6},
                            {3, 2},
                            {3, 5},
                            {3, 6},
                            {4, 2},
                            {4, 5},
                            {4, 6},
                            {5, 2},
                            {5, 3},
                            {5, 4},
                            {5, 5},
                            {5, 6},
                            {6, 2},
                            {6, 3},
                            {6, 4},
                            {6, 5},
                            {6, 6}
                        },
                        "Shape" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10},
                                {10, -1, -1, 10, 10},
                                {10, -1, -1, 10, 10},
                                {10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10}
                            }
                        ],
                        "Shapes" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ]
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, -1, -1, -1, -1, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        }
                                    }
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>
                        },
                        "Colors" -> {5},
                        "Color" -> 5,
                        "Position" -> {2, 2},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10},
                                {10, -1, -1, 10, 10},
                                {10, -1, -1, 10, 10},
                                {10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10}
                            }
                        ],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 5,
                        "Y" -> 2,
                        "X" -> 2,
                        "YInverse" -> 9,
                        "XInverse" -> 9,
                        "Y2" -> 6,
                        "X2" -> 6,
                        "Y2Inverse" -> 5,
                        "X2Inverse" -> 5,
                        "ZOrder" -> 0,
                        "YMiddle" -> 4,
                        "XMiddle" -> 4,
                        "Width" -> 5,
                        "Height" -> 5,
                        "Length" -> 5,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 25,
                        "FilledArea" -> 21,
                        "FilledProportion" -> 0.84,
                        "SurfacePixelCount" -> 16,
                        "VerticalLineSymmetry" -> False,
                        "HorizontalLineSymmetry" -> False,
                        "VerticalAndHorizontalLineSymmetry" -> False,
                        "HollowCount" -> 1,
                        "YRelative" -> 1,
                        "XRelative" -> 1
                    |>
                },
                "SecondMostUsedColor" -> 7,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 3,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 4,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 3,
                "Y.Rank" -> 5,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 5,
                "X.InverseRank" -> 1,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 5,
                "XInverse.Rank" -> 1,
                "XInverse.InverseRank" -> 5,
                "Y2.Rank" -> 2,
                "Y2.InverseRank" -> 3,
                "X2.Rank" -> 3,
                "X2.InverseRank" -> 2,
                "Y2Inverse.Rank" -> 3,
                "Y2Inverse.InverseRank" -> 2,
                "X2Inverse.Rank" -> 2,
                "X2Inverse.InverseRank" -> 3,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 2,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 2,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 2,
                "Area.Rank" -> 1,
                "Area.InverseRank" -> 5,
                "FilledArea.Rank" -> 1,
                "FilledArea.InverseRank" -> 5,
                "FilledProportion.Rank" -> 3,
                "FilledProportion.InverseRank" -> 1,
                "SurfacePixelCount.Rank" -> 1,
                "SurfacePixelCount.InverseRank" -> 5,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 2,
                "MostUsedColor.Rank" -> 4,
                "MostUsedColor.InverseRank" -> 1,
                "SecondMostUsedColor.Rank" -> 1,
                "SecondMostUsedColor.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 2,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 2,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{7, 7}}],
                "PixelPositions" -> {{1, 9}, {1, 10}},
                "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{10, 10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10}, {10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10}, {10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10, 10}, {10, 10, 10, 10}}],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>,
                    <|"Name" -> "Line"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {7},
                "Color" -> 7,
                "Position" -> {1, 9},
                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}}],
                "ColorCount" -> 1,
                "MostUsedColor" -> 7,
                "Y" -> 1,
                "X" -> 9,
                "YInverse" -> 10,
                "XInverse" -> 2,
                "Y2" -> 1,
                "X2" -> 10,
                "Y2Inverse" -> 10,
                "X2Inverse" -> 1,
                "ZOrder" -> 0,
                "YMiddle" -> 1,
                "Width" -> 2,
                "Height" -> 1,
                "Length" -> 2,
                "PrimarySizeDimension" -> "X",
                "AspectRatio" -> 2,
                "Area" -> 2,
                "FilledArea" -> 2,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "ColorUseCount" -> 5,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 2,
                "GeneralShapeUseCount" -> 2,
                "Width.Rank" -> 2,
                "Width.InverseRank" -> 2,
                "Height.Rank" -> 4,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 2,
                "Length.InverseRank" -> 2,
                "Y.Rank" -> 5,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 1,
                "X.InverseRank" -> 5,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 5,
                "XInverse.Rank" -> 5,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 4,
                "Y2.InverseRank" -> 1,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 4,
                "Y2Inverse.Rank" -> 1,
                "Y2Inverse.InverseRank" -> 4,
                "X2Inverse.Rank" -> 4,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 4,
                "YMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 2,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 2,
                "HollowCount.Rank" -> 2,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 4,
                "Area.InverseRank" -> 2,
                "FilledArea.Rank" -> 4,
                "FilledArea.InverseRank" -> 2,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 3,
                "SurfacePixelCount.Rank" -> 4,
                "SurfacePixelCount.InverseRank" -> 2,
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 3,
                "ColorCount.Rank" -> 2,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 2,
                "MostUsedColor.InverseRank" -> 3,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 2,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 2
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{6, 6}}],
                "PixelPositions" -> {{3, 8}, {3, 9}},
                "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{10, 10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10}, {10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10}, {10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10, 10}, {10, 10, 10, 10}}],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>,
                    <|"Name" -> "Line"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {6},
                "Color" -> 6,
                "Position" -> {3, 8},
                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}}],
                "ColorCount" -> 1,
                "MostUsedColor" -> 6,
                "Y" -> 3,
                "X" -> 8,
                "YInverse" -> 8,
                "XInverse" -> 3,
                "Y2" -> 3,
                "X2" -> 9,
                "Y2Inverse" -> 8,
                "X2Inverse" -> 2,
                "ZOrder" -> 0,
                "YMiddle" -> 3,
                "Width" -> 2,
                "Height" -> 1,
                "Length" -> 2,
                "PrimarySizeDimension" -> "X",
                "AspectRatio" -> 2,
                "Area" -> 2,
                "FilledArea" -> 2,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "ColorUseCount" -> 2,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 2,
                "GeneralShapeUseCount" -> 2,
                "Width.Rank" -> 2,
                "Width.InverseRank" -> 2,
                "Height.Rank" -> 4,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 2,
                "Length.InverseRank" -> 2,
                "Y.Rank" -> 4,
                "Y.InverseRank" -> 2,
                "X.Rank" -> 2,
                "X.InverseRank" -> 4,
                "YInverse.Rank" -> 2,
                "YInverse.InverseRank" -> 4,
                "XInverse.Rank" -> 4,
                "XInverse.InverseRank" -> 2,
                "Y2.Rank" -> 3,
                "Y2.InverseRank" -> 2,
                "X2.Rank" -> 2,
                "X2.InverseRank" -> 3,
                "Y2Inverse.Rank" -> 2,
                "Y2Inverse.InverseRank" -> 3,
                "X2Inverse.Rank" -> 3,
                "X2Inverse.InverseRank" -> 2,
                "YMiddle.Rank" -> 3,
                "YMiddle.InverseRank" -> 2,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 2,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 2,
                "HollowCount.Rank" -> 2,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 4,
                "Area.InverseRank" -> 2,
                "FilledArea.Rank" -> 4,
                "FilledArea.InverseRank" -> 2,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 3,
                "SurfacePixelCount.Rank" -> 4,
                "SurfacePixelCount.InverseRank" -> 2,
                "ColorUseCount.Rank" -> 3,
                "ColorUseCount.InverseRank" -> 1,
                "ColorCount.Rank" -> 2,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 3,
                "MostUsedColor.InverseRank" -> 2,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 2,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 2
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{7}}],
                "PixelPositions" -> {{6, 9}},
                "Shape" -> <|"Name" -> "Pixel"|>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10},
                                {10, 10, 10, 10},
                                {10, 10, 10, 10},
                                {10, 10, 10, 10}
                            }
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
                    |>,
                    <|"Name" -> "Pixel"|>,
                    <|"Name" -> "Line"|>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Line", "Angle" -> 90|>,
                    <|"Name" -> "Line", "Angle" -> 135|>,
                    <|"Name" -> "Line", "Angle" -> 45|>,
                    <|"Name" -> "Square", "Filled" -> True|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {7},
                "Color" -> 7,
                "Position" -> {6, 9},
                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                "ColorCount" -> 1,
                "MostUsedColor" -> 7,
                "Y" -> 6,
                "X" -> 9,
                "YInverse" -> 5,
                "XInverse" -> 2,
                "Y2" -> 6,
                "X2" -> 9,
                "Y2Inverse" -> 5,
                "X2Inverse" -> 2,
                "ZOrder" -> 0,
                "YMiddle" -> 6,
                "XMiddle" -> 9,
                "Width" -> 1,
                "Height" -> 1,
                "Length" -> 1,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 1,
                "FilledArea" -> 1,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 1,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "ColorUseCount" -> 5,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 3,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 4,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 3,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 3,
                "Y.InverseRank" -> 3,
                "X.Rank" -> 1,
                "X.InverseRank" -> 5,
                "YInverse.Rank" -> 3,
                "YInverse.InverseRank" -> 3,
                "XInverse.Rank" -> 5,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 2,
                "Y2.InverseRank" -> 3,
                "X2.Rank" -> 2,
                "X2.InverseRank" -> 3,
                "Y2Inverse.Rank" -> 3,
                "Y2Inverse.InverseRank" -> 2,
                "X2Inverse.Rank" -> 3,
                "X2Inverse.InverseRank" -> 2,
                "YMiddle.Rank" -> 2,
                "YMiddle.InverseRank" -> 3,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 2,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 2,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 2,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 5,
                "Area.InverseRank" -> 1,
                "FilledArea.Rank" -> 5,
                "FilledArea.InverseRank" -> 1,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 3,
                "SurfacePixelCount.Rank" -> 5,
                "SurfacePixelCount.InverseRank" -> 1,
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 3,
                "ColorCount.Rank" -> 2,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 2,
                "MostUsedColor.InverseRank" -> 3,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 2,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 2,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[
                    {{7, 5, 5, 5, 5, 5}, {-1, 5, 5, -1, -1, 5}, {-1, 5, 5, 5, 5, 5}}
                ],
                "PixelPositions" -> {
                    {8, 5},
                    {8, 6},
                    {8, 7},
                    {8, 8},
                    {8, 9},
                    {8, 10},
                    {9, 6},
                    {9, 7},
                    {9, 10},
                    {10, 6},
                    {10, 7},
                    {10, 8},
                    {10, 9},
                    {10, 10}
                },
                "Shape" -> Daniel`ARC`ARCScene[
                    {{7, 5, 5, 5, 5, 5}, {-1, 5, 5, -1, -1, 5}, {-1, 5, 5, 5, 5, 5}}
                ],
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{7, 5, 5, 5, 5, 5}, {-1, 5, 5, -1, -1, 5}, {-1, 5, 5, 5, 5, 5}}
                        ]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, 7},
                                {5, 5, 5},
                                {5, 5, 5},
                                {5, -1, 5},
                                {5, -1, 5},
                                {5, 5, 5}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{5, 5, 5, 5, 5, -1}, {5, -1, -1, 5, 5, -1}, {5, 5, 5, 5, 5, 7}}
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {5, 5, 5},
                                {5, -1, 5},
                                {5, -1, 5},
                                {5, 5, 5},
                                {5, 5, 5},
                                {7, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{-1, 5, 5, 5, 5, 5}, {-1, 5, 5, -1, -1, 5}, {7, 5, 5, 5, 5, 5}}
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{5, 5, 5, 5, 5, 7}, {5, -1, -1, 5, 5, -1}, {5, 5, 5, 5, 5, -1}}
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, -1, -1, -1, -1, 5, 5},
                                {-1, -1, 5, 5, 5, 5, -1, -1, -1, -1, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5
                                },
                                {
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    5,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    -1,
                                    5,
                                    5,
                                    5
                                },
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                                {-1, -1, -1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>
                },
                "Colors" -> {5, 7},
                "Position" -> {8, 5},
                "MonochromeImage" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10},
                        {-1, 10, 10, -1, -1, 10},
                        {-1, 10, 10, 10, 10, 10}
                    }
                ],
                "ColorCount" -> 2,
                "MostUsedColor" -> 5,
                "Y" -> 8,
                "X" -> 5,
                "YInverse" -> 3,
                "XInverse" -> 6,
                "Y2" -> 10,
                "X2" -> 10,
                "Y2Inverse" -> 1,
                "X2Inverse" -> 1,
                "ZOrder" -> 0,
                "YMiddle" -> 9,
                "Width" -> 6,
                "Height" -> 3,
                "Length" -> 6,
                "PrimarySizeDimension" -> "X",
                "AspectRatio" -> 2,
                "Area" -> 18,
                "FilledArea" -> 14,
                "FilledProportion" -> 0.7777777777777778,
                "SurfacePixelCount" -> 13,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 1,
                "Components" -> {
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[{{7}}],
                        "PixelPositions" -> {{8, 5}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10},
                                        {10, 10, 10, 10}
                                    }
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
                            |>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Line"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Line", "Angle" -> 0|>,
                            <|"Name" -> "Line", "Angle" -> 90|>,
                            <|"Name" -> "Line", "Angle" -> 135|>,
                            <|"Name" -> "Line", "Angle" -> 45|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {7},
                        "Color" -> 7,
                        "Position" -> {8, 5},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 7,
                        "Y" -> 8,
                        "X" -> 5,
                        "YInverse" -> 3,
                        "XInverse" -> 6,
                        "Y2" -> 8,
                        "X2" -> 5,
                        "Y2Inverse" -> 3,
                        "X2Inverse" -> 6,
                        "ZOrder" -> 0,
                        "YMiddle" -> 8,
                        "XMiddle" -> 5,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "SurfacePixelCount" -> 1,
                        "VerticalLineSymmetry" -> True,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> True,
                        "HollowCount" -> 0,
                        "YRelative" -> 0,
                        "XRelative" -> 0
                    |>,
                    <|
                        "UUID" -> 0,
                        "Image" -> Daniel`ARC`ARCScene[
                            {{5, 5, 5, 5, 5}, {5, 5, -1, -1, 5}, {5, 5, 5, 5, 5}}
                        ],
                        "PixelPositions" -> {
                            {8, 6},
                            {8, 7},
                            {8, 8},
                            {8, 9},
                            {8, 10},
                            {9, 6},
                            {9, 7},
                            {9, 10},
                            {10, 6},
                            {10, 7},
                            {10, 8},
                            {10, 9},
                            {10, 10}
                        },
                        "Shape" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10},
                                {10, 10, -1, -1, 10},
                                {10, 10, 10, 10, 10}
                            }
                        ],
                        "Shapes" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, 10, -1, -1, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ]
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10},
                                        {10, 10, 10},
                                        {10, -1, 10},
                                        {10, -1, 10},
                                        {10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10},
                                        {10, -1, 10},
                                        {10, -1, 10},
                                        {10, 10, 10},
                                        {10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10},
                                        {10, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10},
                                        {10, 10, 10, 10, -1, -1, -1, -1, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                                    }
                                ],
                                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            -1,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        },
                                        {
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10,
                                            10
                                        }
                                    }
                                ],
                                "Transform" -> <|
                                    "Type" -> "Scaled",
                                    "Factor" -> 0.3333333333333333
                                |>
                            |>,
                            <|"Name" -> "Rectangle"|>
                        },
                        "Colors" -> {5},
                        "Color" -> 5,
                        "Position" -> {8, 6},
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10},
                                {10, 10, -1, -1, 10},
                                {10, 10, 10, 10, 10}
                            }
                        ],
                        "ColorCount" -> 1,
                        "MostUsedColor" -> 5,
                        "Y" -> 8,
                        "X" -> 6,
                        "YInverse" -> 3,
                        "XInverse" -> 5,
                        "Y2" -> 10,
                        "X2" -> 10,
                        "Y2Inverse" -> 1,
                        "X2Inverse" -> 1,
                        "ZOrder" -> 0,
                        "YMiddle" -> 9,
                        "XMiddle" -> 8,
                        "Width" -> 5,
                        "Height" -> 3,
                        "Length" -> 5,
                        "PrimarySizeDimension" -> "X",
                        "AspectRatio" -> 5/3,
                        "Area" -> 15,
                        "FilledArea" -> 13,
                        "FilledProportion" -> 0.8666666666666667,
                        "SurfacePixelCount" -> 12,
                        "VerticalLineSymmetry" -> False,
                        "HorizontalLineSymmetry" -> True,
                        "VerticalAndHorizontalLineSymmetry" -> False,
                        "HollowCount" -> 1,
                        "YRelative" -> 0,
                        "XRelative" -> 1
                    |>
                },
                "SecondMostUsedColor" -> 7,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 3,
                "Height.Rank" -> 2,
                "Height.InverseRank" -> 3,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 3,
                "Y.Rank" -> 2,
                "Y.InverseRank" -> 4,
                "X.Rank" -> 3,
                "X.InverseRank" -> 3,
                "YInverse.Rank" -> 4,
                "YInverse.InverseRank" -> 2,
                "XInverse.Rank" -> 3,
                "XInverse.InverseRank" -> 3,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 4,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 4,
                "Y2Inverse.Rank" -> 4,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 4,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 4,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 2,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 2,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 2,
                "Area.Rank" -> 2,
                "Area.InverseRank" -> 4,
                "FilledArea.Rank" -> 2,
                "FilledArea.InverseRank" -> 4,
                "FilledProportion.Rank" -> 2,
                "FilledProportion.InverseRank" -> 2,
                "SurfacePixelCount.Rank" -> 2,
                "SurfacePixelCount.InverseRank" -> 4,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 2,
                "MostUsedColor.Rank" -> 4,
                "MostUsedColor.InverseRank" -> 1,
                "SecondMostUsedColor.Rank" -> 1,
                "SecondMostUsedColor.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 2,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 2,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{8, 8}, {8, 8}}],
                "PixelPositions" -> {{9, 2}, {9, 3}, {10, 2}, {10, 3}},
                "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10}}],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 2.|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10},
                                {10, 10, 10, 10},
                                {10, 10, 10, 10},
                                {10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.25|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.2|>
                    |>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Square", "Filled" -> True|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {8},
                "Color" -> 8,
                "Position" -> {9, 2},
                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                "ColorCount" -> 1,
                "MostUsedColor" -> 8,
                "Y" -> 9,
                "X" -> 2,
                "YInverse" -> 2,
                "XInverse" -> 9,
                "Y2" -> 10,
                "X2" -> 3,
                "Y2Inverse" -> 1,
                "X2Inverse" -> 8,
                "ZOrder" -> 0,
                "Width" -> 2,
                "Height" -> 2,
                "Length" -> 2,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 4,
                "FilledArea" -> 4,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 4,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0,
                "ColorUseCount" -> 4,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 2,
                "Width.InverseRank" -> 2,
                "Height.Rank" -> 3,
                "Height.InverseRank" -> 2,
                "Length.Rank" -> 2,
                "Length.InverseRank" -> 2,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 5,
                "X.Rank" -> 4,
                "X.InverseRank" -> 2,
                "YInverse.Rank" -> 5,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 2,
                "XInverse.InverseRank" -> 4,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 4,
                "X2.Rank" -> 4,
                "X2.InverseRank" -> 1,
                "Y2Inverse.Rank" -> 4,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 1,
                "X2Inverse.InverseRank" -> 4,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 2,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 2,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 2,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 3,
                "Area.InverseRank" -> 3,
                "FilledArea.Rank" -> 3,
                "FilledArea.InverseRank" -> 3,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 3,
                "SurfacePixelCount.Rank" -> 3,
                "SurfacePixelCount.InverseRank" -> 3,
                "ColorUseCount.Rank" -> 2,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 2,
                "ColorCount.InverseRank" -> 1,
                "MostUsedColor.Rank" -> 1,
                "MostUsedColor.InverseRank" -> 4,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 2,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 2,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>
        },
        "ObjectCount" -> 6,
        "Colors" -> {5, 6, 7, 8},
        "ColorCount" -> 4,
        "MostUsedColor" -> 5,
        "SecondMostUsedColor" -> 7,
        "Length" -> 10,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 100,
        "FilledArea" -> 45,
        "FilledProportion" -> 0.45,
        "SurfacePixelCount" -> 30,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 2
    |>
    ,
    TestID -> "ARCParseScene-20220718-9SB5XL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["9565186b"]["Train", 1, "Input"],
                    "FormMultiColorCompositeObjects" -> False
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 3,
        "Height" -> 3,
        "ObjectCount" -> 3,
        "Objects" -> {
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{1}}],
                "PixelPositions" -> {{2, 2}},
                "Shape" -> <|"Name" -> "Pixel"|>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|"Name" -> "Pixel"|>,
                    <|"Name" -> "Line"|>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>,
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Line", "Angle" -> 90|>,
                    <|"Name" -> "Line", "Angle" -> 135|>,
                    <|"Name" -> "Line", "Angle" -> 45|>,
                    <|"Name" -> "Square", "Filled" -> True|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "Colors" -> {1},
                "Color" -> 1,
                "Position" -> {2, 2},
                "ColorCount" -> 1,
                "Y" -> 2,
                "X" -> 2,
                "YInverse" -> 2,
                "XInverse" -> 2,
                "Y2" -> 2,
                "X2" -> 2,
                "Y2Inverse" -> 2,
                "X2Inverse" -> 2,
                "ZOrder" -> 0,
                "YMiddle" -> 2,
                "XMiddle" -> 2,
                "Width" -> 1,
                "Height" -> 1,
                "Length" -> 1,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 1,
                "FilledArea" -> 1,
                "FilledProportion" -> 1.,
                "SurfacePixelCount" -> 1,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 1,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 1,
                "Width.Rank" -> 3,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 3,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 3,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 2,
                "X.Rank" -> 1,
                "X.InverseRank" -> 2,
                "YInverse.Rank" -> 2,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 2,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 2,
                "Y2.InverseRank" -> 1,
                "X2.Rank" -> 2,
                "X2.InverseRank" -> 1,
                "Y2Inverse.Rank" -> 1,
                "Y2Inverse.InverseRank" -> 2,
                "X2Inverse.Rank" -> 1,
                "X2Inverse.InverseRank" -> 2,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 1,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 3,
                "Area.InverseRank" -> 1,
                "FilledArea.Rank" -> 3,
                "FilledArea.InverseRank" -> 1,
                "FilledProportion.Rank" -> 1,
                "FilledProportion.InverseRank" -> 3,
                "SurfacePixelCount.Rank" -> 3,
                "SurfacePixelCount.InverseRank" -> 1,
                "ColorUseCount.Rank" -> 3,
                "ColorUseCount.InverseRank" -> 1,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 2,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}],
                "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}},
                "Shape" -> <|
                    "Name" -> "L",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}
                        ]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}
                        ],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, -1, -1, -1, -1},
                                {10, 10, -1, -1, -1, -1},
                                {10, 10, -1, -1, -1, -1},
                                {10, 10, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10, 10, 10, 10},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1},
                                {10, 10, 10, -1, -1, -1, -1, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|"Name" -> "L"|>,
                    <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>
                },
                "Colors" -> {2},
                "Color" -> 2,
                "Position" -> {1, 1},
                "ColorCount" -> 1,
                "Y" -> 1,
                "X" -> 1,
                "YInverse" -> 3,
                "XInverse" -> 3,
                "Y2" -> 3,
                "X2" -> 3,
                "Y2Inverse" -> 1,
                "X2Inverse" -> 1,
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
                "ColorUseCount" -> 5,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 2,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 3,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 3,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 3,
                "Y.Rank" -> 2,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 2,
                "X.InverseRank" -> 1,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 2,
                "XInverse.Rank" -> 1,
                "XInverse.InverseRank" -> 2,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 2,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 2,
                "Y2Inverse.Rank" -> 2,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 2,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 1,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 1,
                "Area.InverseRank" -> 3,
                "FilledArea.Rank" -> 1,
                "FilledArea.InverseRank" -> 3,
                "FilledProportion.Rank" -> 3,
                "FilledProportion.InverseRank" -> 1,
                "SurfacePixelCount.Rank" -> 1,
                "SurfacePixelCount.InverseRank" -> 3,
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 3,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 2
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{-1, 8}, {8, 8}}],
                "PixelPositions" -> {{2, 3}, {3, 2}, {3, 3}},
                "Shape" -> <|
                    "Name" -> "L",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}}]|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, -1}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}}],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}}],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}}],
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, 10, 10},
                                {-1, -1, 10, 10},
                                {10, 10, 10, 10},
                                {10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, -1, 10, 10, 10},
                                {-1, -1, -1, 10, 10, 10},
                                {-1, -1, -1, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10},
                                {10, 10, 10, 10, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                    |>,
                    <|"Name" -> "L"|>,
                    <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                    |>
                },
                "Colors" -> {8},
                "Color" -> 8,
                "Position" -> {2, 2},
                "ColorCount" -> 1,
                "Y" -> 2,
                "X" -> 2,
                "YInverse" -> 2,
                "XInverse" -> 2,
                "Y2" -> 3,
                "X2" -> 3,
                "Y2Inverse" -> 1,
                "X2Inverse" -> 1,
                "ZOrder" -> 0,
                "Width" -> 2,
                "Height" -> 2,
                "Length" -> 2,
                "PrimarySizeDimension" -> "None",
                "AspectRatio" -> 1,
                "Area" -> 4,
                "FilledArea" -> 3,
                "FilledProportion" -> 0.75,
                "SurfacePixelCount" -> 3,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 3,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 1,
                "GeneralShapeUseCount" -> 2,
                "Width.Rank" -> 2,
                "Width.InverseRank" -> 2,
                "Height.Rank" -> 2,
                "Height.InverseRank" -> 2,
                "Length.Rank" -> 2,
                "Length.InverseRank" -> 2,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 2,
                "X.Rank" -> 1,
                "X.InverseRank" -> 2,
                "YInverse.Rank" -> 2,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 2,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 2,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 2,
                "Y2Inverse.Rank" -> 2,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 2,
                "X2Inverse.InverseRank" -> 1,
                "ZOrder.Rank" -> 1,
                "ZOrder.InverseRank" -> 1,
                "PrimarySizeDimension.Rank" -> 1,
                "PrimarySizeDimension.InverseRank" -> 1,
                "AspectRatio.Rank" -> 1,
                "AspectRatio.InverseRank" -> 1,
                "HollowCount.Rank" -> 1,
                "HollowCount.InverseRank" -> 1,
                "Area.Rank" -> 2,
                "Area.InverseRank" -> 2,
                "FilledArea.Rank" -> 2,
                "FilledArea.InverseRank" -> 2,
                "FilledProportion.Rank" -> 2,
                "FilledProportion.InverseRank" -> 2,
                "SurfacePixelCount.Rank" -> 2,
                "SurfacePixelCount.InverseRank" -> 2,
                "ColorUseCount.Rank" -> 2,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 2
            |>
        },
        "Scene" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}]
    |>
    ,
    TestID -> "ARCParseScene-20220724-3ZJ2P8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["b60334d2"]["Test", 1, "Output"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 9,
        "Height" -> 9,
        "ObjectCount" -> 4,
        "Objects" -> {
            <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}],
                "Position" -> {1, 1}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}],
                "Position" -> {3, 4}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}],
                "Position" -> {5, 7}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}],
                "Position" -> {7, 2}
            |>
        },
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {5, 1, 5, 0, 0, 0, 0, 0, 0},
                {1, 0, 1, 0, 0, 0, 0, 0, 0},
                {5, 1, 5, 5, 1, 5, 0, 0, 0},
                {0, 0, 0, 1, 0, 1, 0, 0, 0},
                {0, 0, 0, 5, 1, 5, 5, 1, 5},
                {0, 0, 0, 0, 0, 0, 1, 0, 1},
                {0, 5, 1, 5, 0, 0, 5, 1, 5},
                {0, 1, 0, 1, 0, 0, 0, 0, 0},
                {0, 5, 1, 5, 0, 0, 0, 0, 0}
            }
        ]
    |>
    ,
    TestID -> "ARCParseScene-20220725-1VFZWR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["321b1fc6"]["Train", 2, "Input"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 10,
        "Height" -> 10,
        "ObjectCount" -> 4,
        "Objects" -> {
            <|
                "Image" -> Daniel`ARC`ARCScene[{{8, 8, -1}, {8, 8, 8}}],
                "Position" -> {2, 2}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{8, 8, -1}, {8, 8, 8}}],
                "Position" -> {3, 7}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{8, 8, -1}, {8, 8, 8}}],
                "Position" -> {8, 4}
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{7, 7, -1}, {6, 6, 6}}],
                "Position" -> {6, 6}
            |>
        },
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 8, 8, 0, 0, 0, 0, 0, 0, 0},
                {0, 8, 8, 8, 0, 0, 8, 8, 0, 0},
                {0, 0, 0, 0, 0, 0, 8, 8, 8, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 7, 7, 0, 0, 0},
                {0, 0, 0, 0, 0, 6, 6, 6, 0, 0},
                {0, 0, 0, 8, 8, 0, 0, 0, 0, 0},
                {0, 0, 0, 8, 8, 8, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
            }
        ]
    |>
    ,
    TestID -> "ARCParseScene-20220725-ACIGXC"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["ihiz27k2n"]["Train", 1, "Output"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 5,
        "Width" -> 10,
        "Height" -> 10,
        "ObjectCount" -> 4,
        "Objects" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {2, 2}|>,
            <|"Image" -> Daniel`ARC`ARCScene[{{3}}], "Position" -> {2, 9}|>,
            <|"Image" -> Daniel`ARC`ARCScene[{{3}}], "Position" -> {9, 2}|>,
            <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {9, 9}|>
        },
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 1, 5, 5, 5, 5, 5, 5, 3, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
                {5, 3, 5, 5, 5, 5, 5, 5, 1, 5},
                {5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
            }
        ]
    |>
    ,
    TestID -> "ARCParseScene-20220806-1M38YL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["178fcbfb"]["Train", 1, "Output"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 9,
        "Height" -> 9,
        "ObjectCount" -> 3,
        "Objects" -> {
            <|
                "Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}}],
                "Position" -> {1, 3},
                "ZOrder" -> 1
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1, 1, 1, 1, 1, 1}}],
                "Position" -> {7, 1},
                "ZOrder" -> 0
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{3, 3, 3, 3, 3, 3, 3, 3, 3}}],
                "Position" -> {5, 1},
                "ZOrder" -> 0
            |>
        },
        "Scene" -> Daniel`ARC`ARCScene[
            {
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {3, 3, 3, 3, 3, 3, 3, 3, 3},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {1, 1, 1, 1, 1, 1, 1, 1, 1},
                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                {0, 0, 2, 0, 0, 0, 0, 0, 0}
            }
        ]
    |>
    ,
    TestID -> "ARCParseScene-20220827-6TVDEY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> {"PixelPositions", "FilledArea"}][
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["ed36ccf7"]["Train", 2, "Input"],
                        "SingleObject" -> True
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 3,
        "Height" -> 3,
        "ObjectCount" -> 1,
        "Objects" -> {
            <|
                "Image" -> Daniel`ARC`ARCScene[{{6, 6, 6}, {-1, -1, -1}, {6, 6, -1}}],
                "Position" -> {1, 1},
                "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {3, 1}, {3, 2}},
                "FilledArea" -> 5
            |>
        },
        "Scene" -> Daniel`ARC`ARCScene[{{6, 6, 6}, {0, 0, 0}, {6, 6, 0}}]
    |>
    ,
    TestID -> "ARCParseScene-20220902-M55N6G"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Module[
                    {res},
                    res = Daniel`ARC`SimplifyObjects["ExtraKeys" -> "GridPosition"][
                        Daniel`ARC`ARCParseScene[
                            Daniel`ARC`ARCParseFile["272f95fa"]["Train", 1, "Output"]
                        ]
                    ];
                    Daniel`ARC`ARCIndent[KeyTake[res, {"Grid", "Objects"}]]
                ]
            ]
        ]
    ]
    ,
    Column[
        {
            "<|",
            "    \"Grid\" -> <|",
            "        \"RowCount\" -> 3,",
            "        \"ColumnCount\" -> 3,",
            Row[
                {
                    Row[
                        {
                            "        ",
                            Row[
                                {
                                    "\"Color\" -> ",
                                    RGBColor[0.4980392156862745, 0.8588235294117647, 1.]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Cells\" -> {",
            "            {",
            "                <|\"Y\" -> 1, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 2|>,",
            "                <|\"Y\" -> 1, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 2|>,",
            "                <|\"Y\" -> 1, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 2|>",
            "            },",
            "            {",
            "                <|\"Y\" -> 4, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 4|>,",
            "                <|\"Y\" -> 4, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 4|>,",
            "                <|\"Y\" -> 4, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 4|>",
            "            },",
            "            {",
            "                <|\"Y\" -> 9, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 10|>,",
            "                <|\"Y\" -> 9, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 10|>,",
            "                <|\"Y\" -> 9, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 10|>",
            "            }",
            "        }",
            "    |>,",
            "    \"Objects\" -> {",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            \"Image\" -> ",
                            Daniel`ARC`ARCScene[
                                {
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8,
                                        8
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    },
                                    {
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        8,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1,
                                        -1
                                    }
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {1, 1},",
            "            \"GridOrDivider\" -> <|",
            "                \"Type\" -> \"Grid\",",
            "                \"RowCount\" -> 3,",
            "                \"ColumnCount\" -> 3,",
            Row[
                {
                    Row[
                        {
                            "                ",
                            Row[
                                {
                                    "\"Color\" -> ",
                                    RGBColor[0.4980392156862745, 0.8588235294117647, 1.]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "                \"Cells\" -> {",
            "                    {",
            "                        <|\"Y\" -> 1, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 2|>,",
            "                        <|\"Y\" -> 1, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 2|>,",
            "                        <|\"Y\" -> 1, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 2|>",
            "                    },",
            "                    {",
            "                        <|\"Y\" -> 4, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 4|>,",
            "                        <|\"Y\" -> 4, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 4|>,",
            "                        <|\"Y\" -> 4, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 4|>",
            "                    },",
            "                    {",
            "                        <|\"Y\" -> 9, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 10|>,",
            "                        <|\"Y\" -> 9, \"X\" -> 6, \"Width\" -> 6, \"Height\" -> 10|>,",
            "                        <|\"Y\" -> 9, \"X\" -> 13, \"Width\" -> 7, \"Height\" -> 10|>",
            "                    }",
            "                }",
            "            |>",
            "        |>,",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {{2, 2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2}}
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {1, 6},",
            "            \"GridPosition\" -> {1, 2}",
            "        |>,",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {
                                            {4, 4, 4, 4},
                                            {4, 4, 4, 4},
                                            {4, 4, 4, 4},
                                            {4, 4, 4, 4}
                                        }
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {4, 1},",
            "            \"GridPosition\" -> {2, 1}",
            "        |>,",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            \"Image\" -> ",
                            Daniel`ARC`ARCScene[
                                {
                                    {6, 6, 6, 6, 6, 6},
                                    {6, 6, 6, 6, 6, 6},
                                    {6, 6, 6, 6, 6, 6},
                                    {6, 6, 6, 6, 6, 6}
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {4, 6},",
            "            \"GridPosition\" -> {2, 2}",
            "        |>,",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            \"Image\" -> ",
                            Daniel`ARC`ARCScene[
                                {
                                    {3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3}
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {4, 13},",
            "            \"GridPosition\" -> {2, 3}",
            "        |>,",
            "        <|",
            Row[
                {
                    Row[
                        {
                            "            \"Image\" -> ",
                            Daniel`ARC`ARCScene[
                                {
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1},
                                    {1, 1, 1, 1, 1, 1}
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Position\" -> {9, 6},",
            "            \"GridPosition\" -> {3, 2}",
            "        |>",
            "    }",
            "|>"
        }
    ]
    ,
    TestID -> "ARCParseScene-20220910-6XK3AR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Module[
                    {res},
                    res = Daniel`ARC`SimplifyObjects["ExtraKeys" -> "GridPosition"][
                        Daniel`ARC`ARCParseScene[
                            Daniel`ARC`ARCParseFile["0bb8dee"]["Train", 2, "Input"]
                        ]["Objects"]
                    ];
                    Daniel`ARC`ARCIndent[res]
                ]
            ]
        ]
    ]
    ,
    Column[
        {
            "{",
            "    <|",
            Row[
                {
                    Row[
                        {
                            "        \"Image\" -> ",
                            Daniel`ARC`ARCScene[
                                {
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {-1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1}
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Position\" -> {1, 1},",
            "        \"GridOrDivider\" -> <|",
            "            \"Type\" -> \"Grid\",",
            "            \"RowCount\" -> 2,",
            "            \"ColumnCount\" -> 2,",
            Row[
                {
                    Row[
                        {
                            "            ",
                            Row[
                                {
                                    "\"Color\" -> ",
                                    RGBColor[0., 0.4549019607843137, 0.8509803921568627]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "            \"Cells\" -> {",
            "                {",
            "                    <|\"Y\" -> 1, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 9|>,",
            "                    <|\"Y\" -> 1, \"X\" -> 6, \"Width\" -> 8, \"Height\" -> 9|>",
            "                },",
            "                {",
            "                    <|\"Y\" -> 11, \"X\" -> 1, \"Width\" -> 4, \"Height\" -> 5|>,",
            "                    <|\"Y\" -> 11, \"X\" -> 6, \"Width\" -> 8, \"Height\" -> 5|>",
            "                }",
            "            }",
            "        |>",
            "    |>,",
            "    <|",
            Row[
                {
                    Row[
                        {
                            "        ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {{3, -1, -1}, {-1, 3, 3}, {-1, 3, -1}}
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Position\" -> {3, 8},",
            "        \"GridPosition\" -> {1, 2}",
            "    |>,",
            "    <|",
            Row[
                {
                    Row[
                        {
                            "        ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {{-1, 2, 2}, {2, 2, 2}, {-1, 2, -1}}
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Position\" -> {6, 1},",
            "        \"GridPosition\" -> {1, 1}",
            "    |>,",
            "    <|",
            Row[
                {
                    Row[
                        {
                            "        ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {{-1, -1, 5}, {5, 5, 5}, {-1, 5, -1}}
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Position\" -> {12, 1},",
            "        \"GridPosition\" -> {2, 1}",
            "    |>,",
            "    <|",
            Row[
                {
                    Row[
                        {
                            "        ",
                            Row[
                                {
                                    "\"Image\" -> ",
                                    Daniel`ARC`ARCScene[
                                        {{8, -1, 8}, {-1, 8, -1}, {8, -1, -1}}
                                    ]
                                }
                            ]
                        }
                    ],
                    ","
                }
            ],
            "        \"Position\" -> {13, 8},",
            "        \"GridPosition\" -> {2, 2}",
            "    |>",
            "}"
        }
    ]
    ,
    TestID -> "ARCParseScene-20220924-THMKQY"
]