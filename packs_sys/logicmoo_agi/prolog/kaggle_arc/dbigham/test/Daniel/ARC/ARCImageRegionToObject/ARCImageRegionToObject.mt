(*
    Tests for: Daniel`ARC`ARCImageRegionToObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageRegionToObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{2, 2, 2}, {2, -1, -1}, {2, -1, -1}},
                        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}}
                    |>,
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}],
        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}},
        "Shape" -> <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
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
            <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
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
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 3,
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
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCImageRegionToObject-20220717-LUDS9W"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{2, 2, 2}, {2, 2, 2}, {2, 2, 2}},
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
                        }
                    |>,
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 2, 2}, {2, 2, 2}}],
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
        "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10}}],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 3.|>
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
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "Square"|>,
            <|"Name" -> "Rectangle"|>,
            <|"Name" -> "Square", "Filled" -> True|>,
            <|"Name" -> "Rectangle", "Filled" -> True|>
        },
        "Colors" -> {2},
        "Color" -> 2,
        "Position" -> {1, 1},
        "ColorCount" -> 1,
        "Y" -> 1,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 3,
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
        "FilledArea" -> 9,
        "FilledProportion" -> 1.,
        "SurfacePixelCount" -> 8,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCImageRegionToObject-20220717-56GZCQ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{2}},
                        "PixelPositions" -> {{1, 1}}
                    |>,
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{2}}],
        "PixelPositions" -> {{1, 1}},
        "Shape" -> <|"Name" -> "Pixel"|>,
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, 10}}],
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
        "Colors" -> {2},
        "Color" -> 2,
        "Position" -> {1, 1},
        "ColorCount" -> 1,
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
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCImageRegionToObject-20220811-XV0TR0"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{2, 2, 2}, {2, 2, 2}, {2, 2, -1}},
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
                        }
                    |>,
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 2, 2}, {2, 2, -1}}],
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
        "Shape" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, -1}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {10, 10, -1}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {-1, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, 10, 10}, {10, 10, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, -1}, {10, 10, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, -1}, {10, 10, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, 10}, {-1, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, -1, -1},
                        {10, 10, 10, 10, -1, -1}
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
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10, -1, -1, -1}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>
        },
        "Colors" -> {2},
        "Color" -> 2,
        "Position" -> {1, 1},
        "ColorCount" -> 1,
        "Y" -> 1,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 3,
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
        "FilledArea" -> 9,
        "FilledProportion" -> 1.,
        "SurfacePixelCount" -> 7,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCImageRegionToObject-20220811-XVU8DU"
]