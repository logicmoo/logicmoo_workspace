(*
    Tests for: Daniel`ARC`ARCPruneOutputsForRuleFinding
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPruneOutputsForRuleFinding]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                ERPTesting`NormalizeOutput["Key" -> "OutputComponentUUID"][
                    Daniel`ARC`ARCPruneOutputsForRuleFinding[
                        Utility`ReturnIfFailure[
                            Daniel`ARC`ARCFindObjectMapping[
                                Daniel`ARC`ARCParseFile[file = "0ca9ddb6"]["Train", 2]
                            ]
                        ],
                        1
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{8}}],
            "PixelPositions" -> {{1, 4}},
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
            "Colors" -> {8},
            "Color" -> 8,
            "Position" -> {1, 4},
            "ColorCount" -> 1,
            "Y" -> 1,
            "X" -> 4,
            "YInverse" -> 9,
            "XInverse" -> 6,
            "Y2" -> 1,
            "X2" -> 4,
            "Y2Inverse" -> 9,
            "X2Inverse" -> 6,
            "ZOrder" -> 0,
            "YMiddle" -> 1,
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
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0,
            "ColorUseCount" -> 1,
            "ImageUseCount" -> 1,
            "ShapeUseCount" -> 5,
            "GeneralShapeUseCount" -> 5,
            "Width.Rank" -> 1,
            "Width.InverseRank" -> 1,
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 5,
            "Y.InverseRank" -> 1,
            "X.Rank" -> 2,
            "X.InverseRank" -> 3,
            "YInverse.Rank" -> 1,
            "YInverse.InverseRank" -> 5,
            "XInverse.Rank" -> 3,
            "XInverse.InverseRank" -> 2,
            "Y2.Rank" -> 5,
            "Y2.InverseRank" -> 1,
            "X2.Rank" -> 2,
            "X2.InverseRank" -> 3,
            "Y2Inverse.Rank" -> 1,
            "Y2Inverse.InverseRank" -> 5,
            "X2Inverse.Rank" -> 3,
            "X2Inverse.InverseRank" -> 2,
            "YMiddle.Rank" -> 5,
            "YMiddle.InverseRank" -> 1,
            "XMiddle.Rank" -> 2,
            "XMiddle.InverseRank" -> 3,
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
            "ColorUseCount.Rank" -> 2,
            "ColorUseCount.InverseRank" -> 1,
            "ColorCount.Rank" -> 1,
            "ColorCount.InverseRank" -> 1,
            "ImageUseCount.Rank" -> 2,
            "ImageUseCount.InverseRank" -> 1,
            "ShapeUseCount.Rank" -> 1,
            "ShapeUseCount.InverseRank" -> 1,
            "GeneralShapeUseCount.Rank" -> 1,
            "GeneralShapeUseCount.InverseRank" -> 1
        |> -> <|
            "Same" -> True,
            "Image" -> Daniel`ARC`ARCScene[{{8}}],
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
            "Colors" -> {8},
            "Color" -> 8,
            "Position" -> {1, 4},
            "Y" -> 1,
            "X" -> 4,
            "YInverse" -> 9,
            "XInverse" -> 6,
            "X2" -> 4,
            "Y2" -> 1,
            "X2Inverse" -> 6,
            "Y2Inverse" -> 9,
            "Width" -> 1,
            "Height" -> 1,
            "ZOrder" -> 0,
            "Example" -> 1,
            "Input" -> <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                "PixelPositions" -> {{1, 4}},
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
                "Colors" -> {8},
                "Color" -> 8,
                "Position" -> {1, 4},
                "ColorCount" -> 1,
                "Y" -> 1,
                "X" -> 4,
                "YInverse" -> 9,
                "XInverse" -> 6,
                "Y2" -> 1,
                "X2" -> 4,
                "Y2Inverse" -> 9,
                "X2Inverse" -> 6,
                "ZOrder" -> 0,
                "YMiddle" -> 1,
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
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 1,
                "ImageUseCount" -> 1,
                "ShapeUseCount" -> 5,
                "GeneralShapeUseCount" -> 5,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 5,
                "Y.InverseRank" -> 1,
                "X.Rank" -> 2,
                "X.InverseRank" -> 3,
                "YInverse.Rank" -> 1,
                "YInverse.InverseRank" -> 5,
                "XInverse.Rank" -> 3,
                "XInverse.InverseRank" -> 2,
                "Y2.Rank" -> 5,
                "Y2.InverseRank" -> 1,
                "X2.Rank" -> 2,
                "X2.InverseRank" -> 3,
                "Y2Inverse.Rank" -> 1,
                "Y2Inverse.InverseRank" -> 5,
                "X2Inverse.Rank" -> 3,
                "X2Inverse.InverseRank" -> 2,
                "YMiddle.Rank" -> 5,
                "YMiddle.InverseRank" -> 1,
                "XMiddle.Rank" -> 2,
                "XMiddle.InverseRank" -> 3,
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
                "ColorUseCount.Rank" -> 2,
                "ColorUseCount.InverseRank" -> 1,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 2,
                "ImageUseCount.InverseRank" -> 1,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1
            |>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2}}],
            "PixelPositions" -> {{3, 7}},
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
            "Position" -> {3, 7},
            "ColorCount" -> 1,
            "Y" -> 3,
            "X" -> 7,
            "YInverse" -> 7,
            "XInverse" -> 3,
            "Y2" -> 3,
            "X2" -> 7,
            "Y2Inverse" -> 7,
            "X2Inverse" -> 3,
            "ZOrder" -> 0,
            "YMiddle" -> 3,
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
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0,
            "ColorUseCount" -> 2,
            "ImageUseCount" -> 2,
            "ShapeUseCount" -> 5,
            "GeneralShapeUseCount" -> 5,
            "Width.Rank" -> 1,
            "Width.InverseRank" -> 1,
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 4,
            "Y.InverseRank" -> 2,
            "X.Rank" -> 1,
            "X.InverseRank" -> 4,
            "YInverse.Rank" -> 2,
            "YInverse.InverseRank" -> 4,
            "XInverse.Rank" -> 4,
            "XInverse.InverseRank" -> 1,
            "Y2.Rank" -> 4,
            "Y2.InverseRank" -> 2,
            "X2.Rank" -> 1,
            "X2.InverseRank" -> 4,
            "Y2Inverse.Rank" -> 2,
            "Y2Inverse.InverseRank" -> 4,
            "X2Inverse.Rank" -> 4,
            "X2Inverse.InverseRank" -> 1,
            "YMiddle.Rank" -> 4,
            "YMiddle.InverseRank" -> 2,
            "XMiddle.Rank" -> 1,
            "XMiddle.InverseRank" -> 4,
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
            "ColorUseCount.Rank" -> 1,
            "ColorUseCount.InverseRank" -> 2,
            "ColorCount.Rank" -> 1,
            "ColorCount.InverseRank" -> 1,
            "ImageUseCount.Rank" -> 1,
            "ImageUseCount.InverseRank" -> 2,
            "ShapeUseCount.Rank" -> 1,
            "ShapeUseCount.InverseRank" -> 1,
            "GeneralShapeUseCount.Rank" -> 1,
            "GeneralShapeUseCount.InverseRank" -> 1,
            "OutputComponentUUID" -> _String
        |> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
            "Shape" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}]|>,
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
                |>
            },
            "Position" -> {2, 6},
            "Y" -> 2,
            "X" -> 6,
            "YInverse" -> 8,
            "XInverse" -> 4,
            "X2" -> 8,
            "Y2" -> 4,
            "X2Inverse" -> 2,
            "Y2Inverse" -> 6,
            "Width" -> 3,
            "Height" -> 3,
            "ZOrder" -> 0,
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> -1,
                                "YInverse" -> -1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 2,
                        "X" -> 6,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> 1,
                                "YInverse" -> -1,
                                "XInverse" -> 1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 2,
                        "X" -> 8,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> 1,
                                "X" -> -1,
                                "YInverse" -> 1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 4,
                        "X" -> 6,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> 1,
                                "X" -> 1,
                                "YInverse" -> 1,
                                "XInverse" -> 1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 4,
                        "X" -> 8,
                        "Color" -> 4
                    |>
                }
            |>,
            "Example" -> 1,
            "Input" -> <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{2}}],
                "PixelPositions" -> {{3, 7}},
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
                "Colors" -> {2},
                "Color" -> 2,
                "Position" -> {3, 7},
                "ColorCount" -> 1,
                "Y" -> 3,
                "X" -> 7,
                "YInverse" -> 7,
                "XInverse" -> 3,
                "Y2" -> 3,
                "X2" -> 7,
                "Y2Inverse" -> 7,
                "X2Inverse" -> 3,
                "ZOrder" -> 0,
                "YMiddle" -> 3,
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
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 2,
                "ImageUseCount" -> 2,
                "ShapeUseCount" -> 5,
                "GeneralShapeUseCount" -> 5,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 4,
                "Y.InverseRank" -> 2,
                "X.Rank" -> 1,
                "X.InverseRank" -> 4,
                "YInverse.Rank" -> 2,
                "YInverse.InverseRank" -> 4,
                "XInverse.Rank" -> 4,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 4,
                "Y2.InverseRank" -> 2,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 4,
                "Y2Inverse.Rank" -> 2,
                "Y2Inverse.InverseRank" -> 4,
                "X2Inverse.Rank" -> 4,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 4,
                "YMiddle.InverseRank" -> 2,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 4,
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
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 2,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1,
                "OutputComponentUUID" -> _String
            |>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "PixelPositions" -> {{4, 3}},
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
            "Colors" -> {1},
            "Color" -> 1,
            "Position" -> {4, 3},
            "ColorCount" -> 1,
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
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0,
            "ColorUseCount" -> 2,
            "ImageUseCount" -> 2,
            "ShapeUseCount" -> 5,
            "GeneralShapeUseCount" -> 5,
            "Width.Rank" -> 1,
            "Width.InverseRank" -> 1,
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 3,
            "Y.InverseRank" -> 3,
            "X.Rank" -> 3,
            "X.InverseRank" -> 2,
            "YInverse.Rank" -> 3,
            "YInverse.InverseRank" -> 3,
            "XInverse.Rank" -> 2,
            "XInverse.InverseRank" -> 3,
            "Y2.Rank" -> 3,
            "Y2.InverseRank" -> 3,
            "X2.Rank" -> 3,
            "X2.InverseRank" -> 2,
            "Y2Inverse.Rank" -> 3,
            "Y2Inverse.InverseRank" -> 3,
            "X2Inverse.Rank" -> 2,
            "X2Inverse.InverseRank" -> 3,
            "YMiddle.Rank" -> 3,
            "YMiddle.InverseRank" -> 3,
            "XMiddle.Rank" -> 3,
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
            "SurfacePixelCount.Rank" -> 1,
            "SurfacePixelCount.InverseRank" -> 1,
            "ColorUseCount.Rank" -> 1,
            "ColorUseCount.InverseRank" -> 2,
            "ColorCount.Rank" -> 1,
            "ColorCount.InverseRank" -> 1,
            "ImageUseCount.Rank" -> 1,
            "ImageUseCount.InverseRank" -> 2,
            "ShapeUseCount.Rank" -> 1,
            "ShapeUseCount.InverseRank" -> 1,
            "GeneralShapeUseCount.Rank" -> 1,
            "GeneralShapeUseCount.InverseRank" -> 1,
            "OutputComponentUUID" -> _String
        |> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
            "Shape" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}]|>,
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
                |>
            },
            "Position" -> {3, 2},
            "Y" -> 3,
            "X" -> 2,
            "YInverse" -> 7,
            "XInverse" -> 8,
            "X2" -> 4,
            "Y2" -> 5,
            "X2Inverse" -> 6,
            "Y2Inverse" -> 5,
            "Width" -> 3,
            "Height" -> 3,
            "ZOrder" -> 0,
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> -1,
                                "YInverse" -> -1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                            |>
                        },
                        "Width" -> 3,
                        "Height" -> 3,
                        "Y" -> 3,
                        "X" -> 2,
                        "Color" -> 7
                    |>
                }
            |>,
            "Example" -> 1,
            "Input" -> <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{1}}],
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
                "Position" -> {4, 3},
                "ColorCount" -> 1,
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
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 2,
                "ImageUseCount" -> 2,
                "ShapeUseCount" -> 5,
                "GeneralShapeUseCount" -> 5,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 3,
                "Y.InverseRank" -> 3,
                "X.Rank" -> 3,
                "X.InverseRank" -> 2,
                "YInverse.Rank" -> 3,
                "YInverse.InverseRank" -> 3,
                "XInverse.Rank" -> 2,
                "XInverse.InverseRank" -> 3,
                "Y2.Rank" -> 3,
                "Y2.InverseRank" -> 3,
                "X2.Rank" -> 3,
                "X2.InverseRank" -> 2,
                "Y2Inverse.Rank" -> 3,
                "Y2Inverse.InverseRank" -> 3,
                "X2Inverse.Rank" -> 2,
                "X2Inverse.InverseRank" -> 3,
                "YMiddle.Rank" -> 3,
                "YMiddle.InverseRank" -> 3,
                "XMiddle.Rank" -> 3,
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
                "SurfacePixelCount.Rank" -> 1,
                "SurfacePixelCount.InverseRank" -> 1,
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 2,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1,
                "OutputComponentUUID" -> _String
            |>
        |>,
        <|
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
            "Colors" -> {1},
            "Color" -> 1,
            "Position" -> {7, 7},
            "ColorCount" -> 1,
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
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0,
            "ColorUseCount" -> 2,
            "ImageUseCount" -> 2,
            "ShapeUseCount" -> 5,
            "GeneralShapeUseCount" -> 5,
            "Width.Rank" -> 1,
            "Width.InverseRank" -> 1,
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 2,
            "Y.InverseRank" -> 4,
            "X.Rank" -> 1,
            "X.InverseRank" -> 4,
            "YInverse.Rank" -> 4,
            "YInverse.InverseRank" -> 2,
            "XInverse.Rank" -> 4,
            "XInverse.InverseRank" -> 1,
            "Y2.Rank" -> 2,
            "Y2.InverseRank" -> 4,
            "X2.Rank" -> 1,
            "X2.InverseRank" -> 4,
            "Y2Inverse.Rank" -> 4,
            "Y2Inverse.InverseRank" -> 2,
            "X2Inverse.Rank" -> 4,
            "X2Inverse.InverseRank" -> 1,
            "YMiddle.Rank" -> 2,
            "YMiddle.InverseRank" -> 4,
            "XMiddle.Rank" -> 1,
            "XMiddle.InverseRank" -> 4,
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
            "ColorUseCount.Rank" -> 1,
            "ColorUseCount.InverseRank" -> 2,
            "ColorCount.Rank" -> 1,
            "ColorCount.InverseRank" -> 1,
            "ImageUseCount.Rank" -> 1,
            "ImageUseCount.InverseRank" -> 2,
            "ShapeUseCount.Rank" -> 1,
            "ShapeUseCount.InverseRank" -> 1,
            "GeneralShapeUseCount.Rank" -> 1,
            "GeneralShapeUseCount.InverseRank" -> 1,
            "OutputComponentUUID" -> _String
        |> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
            "Shape" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}]|>,
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
                |>
            },
            "Position" -> {6, 6},
            "Y" -> 6,
            "X" -> 6,
            "YInverse" -> 4,
            "XInverse" -> 4,
            "X2" -> 8,
            "Y2" -> 8,
            "X2Inverse" -> 2,
            "Y2Inverse" -> 2,
            "Width" -> 3,
            "Height" -> 3,
            "ZOrder" -> 0,
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> -1,
                                "YInverse" -> -1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                            |>
                        },
                        "Width" -> 3,
                        "Height" -> 3,
                        "Y" -> 6,
                        "X" -> 6,
                        "Color" -> 7
                    |>
                }
            |>,
            "Example" -> 1,
            "Input" -> <|
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
                "Position" -> {7, 7},
                "ColorCount" -> 1,
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
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0,
                "ColorUseCount" -> 2,
                "ImageUseCount" -> 2,
                "ShapeUseCount" -> 5,
                "GeneralShapeUseCount" -> 5,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 2,
                "Y.InverseRank" -> 4,
                "X.Rank" -> 1,
                "X.InverseRank" -> 4,
                "YInverse.Rank" -> 4,
                "YInverse.InverseRank" -> 2,
                "XInverse.Rank" -> 4,
                "XInverse.InverseRank" -> 1,
                "Y2.Rank" -> 2,
                "Y2.InverseRank" -> 4,
                "X2.Rank" -> 1,
                "X2.InverseRank" -> 4,
                "Y2Inverse.Rank" -> 4,
                "Y2Inverse.InverseRank" -> 2,
                "X2Inverse.Rank" -> 4,
                "X2Inverse.InverseRank" -> 1,
                "YMiddle.Rank" -> 2,
                "YMiddle.InverseRank" -> 4,
                "XMiddle.Rank" -> 1,
                "XMiddle.InverseRank" -> 4,
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
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 2,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1,
                "OutputComponentUUID" -> _String
            |>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2}}],
            "PixelPositions" -> {{8, 2}},
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
            "Position" -> {8, 2},
            "ColorCount" -> 1,
            "Y" -> 8,
            "X" -> 2,
            "YInverse" -> 2,
            "XInverse" -> 8,
            "Y2" -> 8,
            "X2" -> 2,
            "Y2Inverse" -> 2,
            "X2Inverse" -> 8,
            "ZOrder" -> 0,
            "YMiddle" -> 8,
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
            "ColorUseCount" -> 2,
            "ImageUseCount" -> 2,
            "ShapeUseCount" -> 5,
            "GeneralShapeUseCount" -> 5,
            "Width.Rank" -> 1,
            "Width.InverseRank" -> 1,
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 1,
            "Y.InverseRank" -> 5,
            "X.Rank" -> 4,
            "X.InverseRank" -> 1,
            "YInverse.Rank" -> 5,
            "YInverse.InverseRank" -> 1,
            "XInverse.Rank" -> 1,
            "XInverse.InverseRank" -> 4,
            "Y2.Rank" -> 1,
            "Y2.InverseRank" -> 5,
            "X2.Rank" -> 4,
            "X2.InverseRank" -> 1,
            "Y2Inverse.Rank" -> 5,
            "Y2Inverse.InverseRank" -> 1,
            "X2Inverse.Rank" -> 1,
            "X2Inverse.InverseRank" -> 4,
            "YMiddle.Rank" -> 1,
            "YMiddle.InverseRank" -> 5,
            "XMiddle.Rank" -> 4,
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
            "ColorUseCount.Rank" -> 1,
            "ColorUseCount.InverseRank" -> 2,
            "ColorCount.Rank" -> 1,
            "ColorCount.InverseRank" -> 1,
            "ImageUseCount.Rank" -> 1,
            "ImageUseCount.InverseRank" -> 2,
            "ShapeUseCount.Rank" -> 1,
            "ShapeUseCount.InverseRank" -> 1,
            "GeneralShapeUseCount.Rank" -> 1,
            "GeneralShapeUseCount.InverseRank" -> 1,
            "OutputComponentUUID" -> _String
        |> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
            "Shape" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}]|>,
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
                |>
            },
            "Position" -> {7, 1},
            "Y" -> 7,
            "X" -> 1,
            "YInverse" -> 3,
            "XInverse" -> 9,
            "X2" -> 3,
            "Y2" -> 9,
            "X2Inverse" -> 7,
            "Y2Inverse" -> 1,
            "Width" -> 3,
            "Height" -> 3,
            "ZOrder" -> 0,
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> -1,
                                "YInverse" -> -1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 7,
                        "X" -> 1,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> -1,
                                "X" -> 1,
                                "YInverse" -> -1,
                                "XInverse" -> 1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 7,
                        "X" -> 3,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> 1,
                                "X" -> -1,
                                "YInverse" -> 1,
                                "XInverse" -> -1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 9,
                        "X" -> 1,
                        "Color" -> 4
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> 1,
                                "X" -> 1,
                                "YInverse" -> 1,
                                "XInverse" -> 1
                            |>
                        |>,
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
                        "Width" -> 1,
                        "Height" -> 1,
                        "Y" -> 9,
                        "X" -> 3,
                        "Color" -> 4
                    |>
                }
            |>,
            "Example" -> 1,
            "Input" -> <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{2}}],
                "PixelPositions" -> {{8, 2}},
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
                "Colors" -> {2},
                "Color" -> 2,
                "Position" -> {8, 2},
                "ColorCount" -> 1,
                "Y" -> 8,
                "X" -> 2,
                "YInverse" -> 2,
                "XInverse" -> 8,
                "Y2" -> 8,
                "X2" -> 2,
                "Y2Inverse" -> 2,
                "X2Inverse" -> 8,
                "ZOrder" -> 0,
                "YMiddle" -> 8,
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
                "ColorUseCount" -> 2,
                "ImageUseCount" -> 2,
                "ShapeUseCount" -> 5,
                "GeneralShapeUseCount" -> 5,
                "Width.Rank" -> 1,
                "Width.InverseRank" -> 1,
                "Height.Rank" -> 1,
                "Height.InverseRank" -> 1,
                "Length.Rank" -> 1,
                "Length.InverseRank" -> 1,
                "Y.Rank" -> 1,
                "Y.InverseRank" -> 5,
                "X.Rank" -> 4,
                "X.InverseRank" -> 1,
                "YInverse.Rank" -> 5,
                "YInverse.InverseRank" -> 1,
                "XInverse.Rank" -> 1,
                "XInverse.InverseRank" -> 4,
                "Y2.Rank" -> 1,
                "Y2.InverseRank" -> 5,
                "X2.Rank" -> 4,
                "X2.InverseRank" -> 1,
                "Y2Inverse.Rank" -> 5,
                "Y2Inverse.InverseRank" -> 1,
                "X2Inverse.Rank" -> 1,
                "X2Inverse.InverseRank" -> 4,
                "YMiddle.Rank" -> 1,
                "YMiddle.InverseRank" -> 5,
                "XMiddle.Rank" -> 4,
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
                "ColorUseCount.Rank" -> 1,
                "ColorUseCount.InverseRank" -> 2,
                "ColorCount.Rank" -> 1,
                "ColorCount.InverseRank" -> 1,
                "ImageUseCount.Rank" -> 1,
                "ImageUseCount.InverseRank" -> 2,
                "ShapeUseCount.Rank" -> 1,
                "ShapeUseCount.InverseRank" -> 1,
                "GeneralShapeUseCount.Rank" -> 1,
                "GeneralShapeUseCount.InverseRank" -> 1,
                "OutputComponentUUID" -> _String
            |>
        |>
    }
    ,
    TestID -> "ARCPruneOutputsForRuleFinding-20220719-L90VO7"
]