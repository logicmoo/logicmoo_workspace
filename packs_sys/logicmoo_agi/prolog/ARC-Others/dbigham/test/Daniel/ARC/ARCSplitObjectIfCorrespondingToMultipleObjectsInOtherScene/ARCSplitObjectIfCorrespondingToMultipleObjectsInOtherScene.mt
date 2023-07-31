(*
    Tests for: Daniel`ARC`ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene[
                    <|
                        "UUID" -> "09ce3815-e6e9-416b-81e4-6e66d5ed0270",
                        "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, 5, 5}}],
                        "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}, {3, 1}, {3, 2}, {3, 3}},
                        "Colors" -> {5},
                        "Position" -> {2, 1},
                        "Y" -> 2,
                        "X" -> 1,
                        "Y2" -> 3,
                        "X2" -> 3,
                        "Width" -> 3,
                        "Height" -> 2
                    |>,
                    {
                        <|
                            "UUID" -> "ada0ee5f-9bee-4679-89b3-2bc5f13df6e5",
                            "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}}],
                            "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}},
                            "Colors" -> {4},
                            "Position" -> {2, 1},
                            "Y" -> 2,
                            "X" -> 1,
                            "Y2" -> 2,
                            "X2" -> 3,
                            "Width" -> 3,
                            "Height" -> 1
                        |>,
                        <|
                            "UUID" -> "52b2ae0a-6f61-47f6-8065-f3dfa97aa158",
                            "Image" -> Daniel`ARC`ARCScene[{{9, 9, 9}}],
                            "PixelPositions" -> {{3, 1}, {3, 2}, {3, 3}},
                            "Colors" -> {9},
                            "Position" -> {3, 1},
                            "Y" -> 3,
                            "X" -> 1,
                            "Y2" -> 3,
                            "X2" -> 3,
                            "Width" -> 3,
                            "Height" -> 1
                        |>
                    },
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "UUID" -> 0,
            "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}}],
            "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}},
            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{10, 10, 10, 10, 10, 10}, {10, 10, 10, 10, 10, 10}}
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                |>,
                <|"Name" -> "Line"|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Line", "Angle" -> 0|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Colors" -> {5},
            "Color" -> 5,
            "Position" -> {2, 1},
            "ColorCount" -> 1,
            "Y" -> 2,
            "X" -> 1,
            "YInverse" -> 9,
            "XInverse" -> 10,
            "Y2" -> 2,
            "X2" -> 3,
            "Y2Inverse" -> 9,
            "X2Inverse" -> 8,
            "ZOrder" -> 0,
            "YMiddle" -> 2,
            "XMiddle" -> 2,
            "Width" -> 3,
            "Height" -> 1,
            "Length" -> 3,
            "PrimarySizeDimension" -> "X",
            "AspectRatio" -> 3,
            "Area" -> 3,
            "FilledArea" -> 3,
            "FilledProportion" -> 1.,
            "SurfacePixelCount" -> 3,
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0
        |>,
        <|
            "UUID" -> 0,
            "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}}],
            "PixelPositions" -> {{3, 1}, {3, 2}, {3, 3}},
            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{10, 10, 10, 10, 10, 10}, {10, 10, 10, 10, 10, 10}}
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                |>,
                <|"Name" -> "Line"|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Line", "Angle" -> 0|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Colors" -> {5},
            "Color" -> 5,
            "Position" -> {3, 1},
            "ColorCount" -> 1,
            "Y" -> 3,
            "X" -> 1,
            "YInverse" -> 8,
            "XInverse" -> 10,
            "Y2" -> 3,
            "X2" -> 3,
            "Y2Inverse" -> 8,
            "X2Inverse" -> 8,
            "ZOrder" -> 0,
            "YMiddle" -> 3,
            "XMiddle" -> 2,
            "Width" -> 3,
            "Height" -> 1,
            "Length" -> 3,
            "PrimarySizeDimension" -> "X",
            "AspectRatio" -> 3,
            "Area" -> 3,
            "FilledArea" -> 3,
            "FilledProportion" -> 1.,
            "SurfacePixelCount" -> 3,
            "VerticalLineSymmetry" -> False,
            "HorizontalLineSymmetry" -> False,
            "VerticalAndHorizontalLineSymmetry" -> False,
            "HollowCount" -> 0
        |>
    }
    ,
    TestID -> "ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene-20220819-R81OJD"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        ERPTesting`NormalizeOutput[
            Daniel`ARC`ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene[
                <|
                    "UUID" -> "09ce3815-e6e9-416b-81e4-6e66d5ed0270",
                    "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, 5, 5}}],
                    "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}, {3, 1}, {3, 2}, {3, 3}},
                    "Colors" -> {5},
                    "Position" -> {2, 1},
                    "Y" -> 2,
                    "X" -> 1,
                    "Y2" -> 3,
                    "X2" -> 3,
                    "Width" -> 3,
                    "Height" -> 2
                |>,
                {
                    <|
                        "UUID" -> "ada0ee5f-9bee-4679-89b3-2bc5f13df6e5",
                        "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}}],
                        "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}},
                        "Colors" -> {4},
                        "Position" -> {2, 1},
                        "Y" -> 2,
                        "X" -> 1,
                        "Y2" -> 2,
                        "X2" -> 3,
                        "Width" -> 3,
                        "Height" -> 1
                    |>
                },
                10,
                10
            ]
        ]
    ]
    ,
    {
        <|
            "UUID" -> 0,
            "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, 5, 5}}],
            "PixelPositions" -> {{2, 1}, {2, 2}, {2, 3}, {3, 1}, {3, 2}, {3, 3}},
            "Colors" -> {5},
            "Position" -> {2, 1},
            "Y" -> 2,
            "X" -> 1,
            "Y2" -> 3,
            "X2" -> 3,
            "Width" -> 3,
            "Height" -> 2
        |>
    }
    ,
    TestID -> "ARCSplitObjectIfCorrespondingToMultipleObjectsInOtherScene-20220819-KYKL9Q"
]