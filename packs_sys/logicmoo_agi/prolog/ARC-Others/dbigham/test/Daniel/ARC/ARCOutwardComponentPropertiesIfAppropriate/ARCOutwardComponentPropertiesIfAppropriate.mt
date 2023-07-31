(*
    Tests for: Daniel`ARC`ARCOutwardComponentPropertiesIfAppropriate
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCOutwardComponentPropertiesIfAppropriate]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCOutwardComponentPropertiesIfAppropriate[
        <|
            "UUID" -> "1699055b-d715-4dad-a882-a8be620b4ba0",
            "Image" -> Daniel`ARC`ARCScene[
                {{2, -1, -1}, {2, 2, -1}, {1, 2, 2}, {2, 2, -1}, {2, -1, -1}}
            ],
            "PixelPositions" -> {
                {3, 4},
                {4, 4},
                {4, 5},
                {5, 4},
                {5, 5},
                {5, 6},
                {6, 4},
                {6, 5},
                {7, 4}
            },
            "Shape" -> Daniel`ARC`ARCScene[
                {{10, -1, -1}, {10, 10, -1}, {10, 10, 10}, {10, 10, -1}, {10, -1, -1}}
            ],
            "Shapes" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{2, -1, -1}, {2, 2, -1}, {1, 2, 2}, {2, 2, -1}, {2, -1, -1}}
                    ]
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{2, 2, 1, 2, 2}, {-1, 2, 2, 2, -1}, {-1, -1, 2, -1, -1}}
                    ],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{-1, -1, 2}, {-1, 2, 2}, {2, 2, 1}, {-1, 2, 2}, {-1, -1, 2}}
                    ],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{-1, -1, 2, -1, -1}, {-1, 2, 2, 2, -1}, {2, 2, 1, 2, 2}}
                    ],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>
            },
            "Colors" -> {1, 2},
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
            "FilledArea" -> 9,
            "FilledProportion" -> 0.6,
            "Components" -> {
                <|
                    "UUID" -> "7cdcd221-ab79-465c-afdb-8191e8eb58fe",
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
                |>,
                <|
                    "UUID" -> "c185b5d2-735f-4f10-9002-ea26516b4136",
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
                    "WidthRank" -> 2,
                    "WidthInverseRank" -> 1,
                    "HeightRank" -> 2,
                    "HeightInverseRank" -> 1,
                    "LengthRank" -> 2,
                    "LengthInverseRank" -> 1,
                    "YRank" -> 1,
                    "YInverseRank" -> 2,
                    "XRank" -> 1,
                    "XInverseRank" -> 1,
                    "Y2Rank" -> 2,
                    "Y2InverseRank" -> 1,
                    "X2Rank" -> 2,
                    "X2InverseRank" -> 1,
                    "PrimarySizeDimensionRank" -> 2,
                    "PrimarySizeDimensionInverseRank" -> 1,
                    "AspectRatioRank" -> 1,
                    "AspectRatioInverseRank" -> 2
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1, 1, 1, 1, 1, 1}}],
            "Position" -> {5, 7},
            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10, 10, 10, 10, 10, 10, 10}}]|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{10}, {10}, {10}, {10}, {10}, {10}, {10}, {10}, {10}}
                    ],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{10}, {10}, {10}, {10}, {10}, {10}, {10}, {10}, {10}}
                    ],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|"Name" -> "Line"|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Line", "Angle" -> 0|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Colors" -> {1},
            "Width" -> 9,
            "Height" -> 1
        |>,
        <|"Width" -> 15, "Height" -> 10|>
    ]
    ,
    <|"Outward" -> True, "Direction" -> {0, 1}|>
    ,
    TestID -> "ARCOutwardComponentPropertiesIfAppropriate-20220813-OQLTI2"
]