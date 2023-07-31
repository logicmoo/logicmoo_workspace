(*
    Tests for: Daniel`ARC`ARCObjectWithComponent
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectWithComponent]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SimplifyObjects[
        Daniel`ARC`ARCObjectWithComponent[
            {
                <|
                    "UUID" -> "169d47bd-d049-4741-b453-a98b09a4ed78",
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                            {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
                        }
                    ],
                    "PixelPositions" -> {
                        {3, 4},
                        {4, 4},
                        {4, 5},
                        {5, 4},
                        {5, 5},
                        {5, 6},
                        {5, 7},
                        {5, 8},
                        {5, 9},
                        {5, 10},
                        {5, 11},
                        {5, 12},
                        {5, 13},
                        {5, 14},
                        {5, 15},
                        {6, 4},
                        {6, 5},
                        {7, 4}
                    },
                    "Shape" -> Daniel`ARC`ARCScene[
                        {
                            {10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                            {10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
                        }
                    ],
                    "Shapes" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                                    {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                                    {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
                                }
                            ]
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {2, 2, 1, 2, 2},
                                    {-1, 2, 2, 2, -1},
                                    {-1, -1, 2, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1}
                                }
                            ],
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2},
                                    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 2},
                                    {1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1},
                                    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 2},
                                    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2}
                                }
                            ],
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 1, -1, -1},
                                    {-1, -1, 2, -1, -1},
                                    {-1, 2, 2, 2, -1},
                                    {2, 2, 1, 2, 2}
                                }
                            ],
                            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                        |>
                    },
                    "Colors" -> {1, 2},
                    "Position" -> {3, 4},
                    "Y" -> 3,
                    "X" -> 4,
                    "Y2" -> 7,
                    "X2" -> 15,
                    "Width" -> 12,
                    "Height" -> 5,
                    "Length" -> 12,
                    "PrimarySizeDimension" -> "X",
                    "AspectRatio" -> 12/5,
                    "Area" -> 60,
                    "FilledArea" -> 18,
                    "FilledProportion" -> 0.3,
                    "Components" -> {
                        <|
                            "UUID" -> "063a85d8-bcdb-4bad-9d0d-b7258b40df3c",
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {2, -1, -1},
                                    {2, 2, -1},
                                    {-1, 2, 2},
                                    {2, 2, -1},
                                    {2, -1, -1}
                                }
                            ],
                            "PixelPositions" -> {
                                {3, 4},
                                {4, 4},
                                {4, 5},
                                {5, 5},
                                {5, 6},
                                {6, 4},
                                {6, 5},
                                {7, 4}
                            },
                            "Shape" -> Daniel`ARC`ARCScene[
                                {
                                    {10, -1, -1},
                                    {10, 10, -1},
                                    {-1, 10, 10},
                                    {10, 10, -1},
                                    {10, -1, -1}
                                }
                            ],
                            "Shapes" -> {
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {10, -1, -1},
                                            {10, 10, -1},
                                            {-1, 10, 10},
                                            {10, 10, -1},
                                            {10, -1, -1}
                                        }
                                    ]
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {10, 10, -1, 10, 10},
                                            {-1, 10, 10, 10, -1},
                                            {-1, -1, 10, -1, -1}
                                        }
                                    ],
                                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {-1, -1, 10},
                                            {-1, 10, 10},
                                            {10, 10, -1},
                                            {-1, 10, 10},
                                            {-1, -1, 10}
                                        }
                                    ],
                                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {-1, -1, 10, -1, -1},
                                            {-1, 10, 10, 10, -1},
                                            {10, 10, -1, 10, 10}
                                        }
                                    ],
                                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                                |>
                            },
                            "Colors" -> {2},
                            "Position" -> {3, 4},
                            "Y" -> 3,
                            "X" -> 4,
                            "Y2" -> 7,
                            "X2" -> 6,
                            "Width" -> 3,
                            "Height" -> 5,
                            "Length" -> 5,
                            "PrimarySizeDimension" -> "Y",
                            "AspectRatio" -> 3/5,
                            "Area" -> 15,
                            "FilledArea" -> 8,
                            "FilledProportion" -> 0.5333333333333333,
                            "WidthRank" -> 2,
                            "WidthInverseRank" -> 2,
                            "HeightRank" -> 1,
                            "HeightInverseRank" -> 2,
                            "LengthRank" -> 2,
                            "LengthInverseRank" -> 2,
                            "YRank" -> 2,
                            "YInverseRank" -> 1,
                            "XRank" -> 2,
                            "XInverseRank" -> 1,
                            "Y2Rank" -> 1,
                            "Y2InverseRank" -> 2,
                            "X2Rank" -> 2,
                            "X2InverseRank" -> 2,
                            "PrimarySizeDimensionRank" -> 1,
                            "PrimarySizeDimensionInverseRank" -> 3,
                            "AspectRatioRank" -> 3,
                            "AspectRatioInverseRank" -> 1
                        |>,
                        <|
                            "UUID" -> "e88d025e-d735-4339-934d-e1e113b634c3",
                            "Image" -> Daniel`ARC`ARCScene[{{1}}],
                            "PixelPositions" -> {{5, 4}},
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "Shapes" -> {
                                <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                                <|"Name" -> "Pixel"|>,
                                <|"Name" -> "Square"|>,
                                <|"Name" -> "Rectangle"|>,
                                <|"Name" -> "Square", "Filled" -> True|>,
                                <|"Name" -> "Rectangle", "Filled" -> True|>
                            },
                            "Colors" -> {1},
                            "Position" -> {5, 4},
                            "Y" -> 5,
                            "X" -> 4,
                            "Y2" -> 5,
                            "X2" -> 4,
                            "Width" -> 1,
                            "Height" -> 1,
                            "Length" -> 1,
                            "PrimarySizeDimension" -> "None",
                            "AspectRatio" -> 1,
                            "Area" -> 1,
                            "FilledArea" -> 1,
                            "FilledProportion" -> 1.,
                            "WidthRank" -> 3,
                            "WidthInverseRank" -> 1,
                            "HeightRank" -> 2,
                            "HeightInverseRank" -> 1,
                            "LengthRank" -> 3,
                            "LengthInverseRank" -> 1,
                            "YRank" -> 1,
                            "YInverseRank" -> 2,
                            "XRank" -> 2,
                            "XInverseRank" -> 1,
                            "Y2Rank" -> 2,
                            "Y2InverseRank" -> 1,
                            "X2Rank" -> 3,
                            "X2InverseRank" -> 1,
                            "PrimarySizeDimensionRank" -> 3,
                            "PrimarySizeDimensionInverseRank" -> 1,
                            "AspectRatioRank" -> 2,
                            "AspectRatioInverseRank" -> 2
                        |>,
                        <|
                            "UUID" -> "222d7661-7140-42d2-b4a6-20a966a2e035",
                            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1, 1, 1, 1, 1, 1}}],
                            "PixelPositions" -> {
                                {5, 7},
                                {5, 8},
                                {5, 9},
                                {5, 10},
                                {5, 11},
                                {5, 12},
                                {5, 13},
                                {5, 14},
                                {5, 15}
                            },
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                            "Shapes" -> {
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {{10, 10, 10, 10, 10, 10, 10, 10, 10}}
                                    ]
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10}
                                        }
                                    ],
                                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[
                                        {
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10},
                                            {10}
                                        }
                                    ],
                                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                                |>,
                                <|"Name" -> "Line"|>,
                                <|"Name" -> "Rectangle"|>,
                                <|"Name" -> "Line", "Angle" -> 0|>,
                                <|"Name" -> "Rectangle", "Filled" -> True|>
                            },
                            "Colors" -> {1},
                            "Position" -> {5, 7},
                            "Y" -> 5,
                            "X" -> 7,
                            "Y2" -> 5,
                            "X2" -> 15,
                            "Width" -> 9,
                            "Height" -> 1,
                            "Length" -> 9,
                            "PrimarySizeDimension" -> "X",
                            "AspectRatio" -> 9,
                            "Area" -> 9,
                            "FilledArea" -> 9,
                            "FilledProportion" -> 1.,
                            "WidthRank" -> 1,
                            "WidthInverseRank" -> 3,
                            "HeightRank" -> 2,
                            "HeightInverseRank" -> 1,
                            "LengthRank" -> 1,
                            "LengthInverseRank" -> 3,
                            "YRank" -> 1,
                            "YInverseRank" -> 2,
                            "XRank" -> 1,
                            "XInverseRank" -> 2,
                            "Y2Rank" -> 2,
                            "Y2InverseRank" -> 1,
                            "X2Rank" -> 1,
                            "X2InverseRank" -> 3,
                            "PrimarySizeDimensionRank" -> 2,
                            "PrimarySizeDimensionInverseRank" -> 2,
                            "AspectRatioRank" -> 1,
                            "AspectRatioInverseRank" -> 3
                        |>
                    },
                    "WidthRank" -> 1,
                    "WidthInverseRank" -> 1,
                    "HeightRank" -> 1,
                    "HeightInverseRank" -> 1,
                    "LengthRank" -> 1,
                    "LengthInverseRank" -> 1,
                    "YRank" -> 1,
                    "YInverseRank" -> 1,
                    "XRank" -> 1,
                    "XInverseRank" -> 1,
                    "Y2Rank" -> 1,
                    "Y2InverseRank" -> 1,
                    "X2Rank" -> 1,
                    "X2InverseRank" -> 1,
                    "PrimarySizeDimensionRank" -> 1,
                    "PrimarySizeDimensionInverseRank" -> 1,
                    "AspectRatioRank" -> 1,
                    "AspectRatioInverseRank" -> 1
                |>
            },
            <|
                "UUID" -> "6cc4ae7a-679d-47d3-8b6c-132cbedc457a",
                "Image" -> Daniel`ARC`ARCScene[
                    {{2, -1, -1}, {2, 2, -1}, {-1, 2, 2}, {2, 2, -1}, {2, -1, -1}}
                ],
                "PixelPositions" -> {
                    {3, 4},
                    {4, 4},
                    {4, 5},
                    {5, 5},
                    {5, 6},
                    {6, 4},
                    {6, 5},
                    {7, 4}
                },
                "Shape" -> Daniel`ARC`ARCScene[
                    {{10, -1, -1}, {10, 10, -1}, {-1, 10, 10}, {10, 10, -1}, {10, -1, -1}}
                ],
                "Shapes" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, -1, -1},
                                {10, 10, -1},
                                {-1, 10, 10},
                                {10, 10, -1},
                                {10, -1, -1}
                            }
                        ]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, -1, 10, 10},
                                {-1, 10, 10, 10, -1},
                                {-1, -1, 10, -1, -1}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, 10},
                                {-1, 10, 10},
                                {10, 10, -1},
                                {-1, 10, 10},
                                {-1, -1, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[
                            {
                                {-1, -1, 10, -1, -1},
                                {-1, 10, 10, 10, -1},
                                {10, 10, -1, 10, 10}
                            }
                        ],
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                },
                "Colors" -> {2},
                "Position" -> {3, 4},
                "Y" -> 3,
                "X" -> 4,
                "Y2" -> 7,
                "X2" -> 6,
                "Width" -> 3,
                "Height" -> 5,
                "Length" -> 5,
                "PrimarySizeDimension" -> "Y",
                "AspectRatio" -> 3/5,
                "Area" -> 15,
                "FilledArea" -> 8,
                "FilledProportion" -> 0.5333333333333333,
                "WidthRank" -> 1,
                "WidthInverseRank" -> 2,
                "HeightRank" -> 1,
                "HeightInverseRank" -> 2,
                "LengthRank" -> 1,
                "LengthInverseRank" -> 2,
                "YRank" -> 2,
                "YInverseRank" -> 1,
                "XRank" -> 1,
                "XInverseRank" -> 1,
                "Y2Rank" -> 1,
                "Y2InverseRank" -> 2,
                "X2Rank" -> 1,
                "X2InverseRank" -> 2,
                "PrimarySizeDimensionRank" -> 1,
                "PrimarySizeDimensionInverseRank" -> 2,
                "AspectRatioRank" -> 2,
                "AspectRatioInverseRank" -> 1
            |>
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                    {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                    {1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                    {2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                    {2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
                }
            ],
            "Position" -> {3, 4}
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{2, -1, -1}, {2, 2, -1}, {-1, 2, 2}, {2, 2, -1}, {2, -1, -1}}
            ],
            "Position" -> {3, 4}
        |>
    }
    ,
    TestID -> "ARCObjectWithComponent-20220813-JCSRCZ"
]