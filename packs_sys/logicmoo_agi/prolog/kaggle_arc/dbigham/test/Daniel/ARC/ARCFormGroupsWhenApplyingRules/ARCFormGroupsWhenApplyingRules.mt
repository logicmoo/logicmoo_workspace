(*
    Tests for: Daniel`ARC`ARCFormGroupsWhenApplyingRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFormGroupsWhenApplyingRules]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`SimplifyObjects[
            Daniel`ARC`ARCFormGroupsWhenApplyingRules[
                {
                    <|
                        "UUID" -> "afd40a2f-b40c-49a3-91f4-168d636c1ded",
                        "Image" -> Daniel`ARC`ARCScene[{{8}}],
                        "PixelPositions" -> {{4, 3}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {8},
                        "Position" -> {4, 3},
                        "Y" -> 4,
                        "X" -> 3,
                        "Y2" -> 4,
                        "X2" -> 3,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "WidthRank" -> 1,
                        "WidthInverseRank" -> 1,
                        "HeightRank" -> 1,
                        "HeightInverseRank" -> 1,
                        "LengthRank" -> 1,
                        "LengthInverseRank" -> 1,
                        "YRank" -> 1,
                        "YInverseRank" -> 1,
                        "XRank" -> 2,
                        "XInverseRank" -> 1,
                        "Y2Rank" -> 1,
                        "Y2InverseRank" -> 1,
                        "X2Rank" -> 2,
                        "X2InverseRank" -> 1,
                        "PrimarySizeDimensionRank" -> 1,
                        "PrimarySizeDimensionInverseRank" -> 1,
                        "AspectRatioRank" -> 1,
                        "AspectRatioInverseRank" -> 1
                    |>,
                    <|
                        "UUID" -> "e4b601e3-7d7b-48ef-9cab-c1cdf5a8b2a2",
                        "Image" -> Daniel`ARC`ARCScene[{{8}}],
                        "PixelPositions" -> {{4, 10}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Shapes" -> {
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|"Name" -> "Pixel"|>,
                            <|"Name" -> "Square"|>,
                            <|"Name" -> "Rectangle"|>,
                            <|"Name" -> "Square", "Filled" -> True|>,
                            <|"Name" -> "Rectangle", "Filled" -> True|>
                        },
                        "Colors" -> {8},
                        "Position" -> {4, 10},
                        "Y" -> 4,
                        "X" -> 10,
                        "Y2" -> 4,
                        "X2" -> 10,
                        "Width" -> 1,
                        "Height" -> 1,
                        "Length" -> 1,
                        "PrimarySizeDimension" -> "None",
                        "AspectRatio" -> 1,
                        "Area" -> 1,
                        "FilledArea" -> 1,
                        "FilledProportion" -> 1.,
                        "WidthRank" -> 1,
                        "WidthInverseRank" -> 1,
                        "HeightRank" -> 1,
                        "HeightInverseRank" -> 1,
                        "LengthRank" -> 1,
                        "LengthInverseRank" -> 1,
                        "YRank" -> 1,
                        "YInverseRank" -> 1,
                        "XRank" -> 1,
                        "XInverseRank" -> 2,
                        "Y2Rank" -> 1,
                        "Y2InverseRank" -> 1,
                        "X2Rank" -> 1,
                        "X2InverseRank" -> 2,
                        "PrimarySizeDimensionRank" -> 1,
                        "PrimarySizeDimensionInverseRank" -> 1,
                        "AspectRatioRank" -> 1,
                        "AspectRatioInverseRank" -> 1
                    |>
                },
                {
                    <|
                        "Colors" -> {8},
                        "Height" -> 1,
                        "PrimarySizeDimension" -> "X",
                        "FilledArea" -> 2
                    |>,
                    <|
                        "Colors" -> {8},
                        "Width" -> 1,
                        "PrimarySizeDimension" -> "Y",
                        "FilledArea" -> 2
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
            "Image" -> Daniel`ARC`ARCScene[{{8, -1, -1, -1, -1, -1, -1, 8}}],
            "Position" -> {4, 3}
        |>
    }
    ,
    TestID -> "ARCFormGroupsWhenApplyingRules-20220812-0PAYA9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> {"Type"}][
                Daniel`ARC`ARCFormGroupsWhenApplyingRules[
                    {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{1}}],
                            "PixelPositions" -> {{1, 1}},
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "Colors" -> {1},
                            "Color" -> 1,
                            "Position" -> {1, 1},
                            "Y" -> 1,
                            "X" -> 1,
                            "Width" -> 1,
                            "Height" -> 1
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{1}}],
                            "PixelPositions" -> {{3, 3}},
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "Colors" -> {1},
                            "Color" -> 1,
                            "Position" -> {3, 3},
                            "Y" -> 3,
                            "X" -> 3,
                            "Width" -> 1,
                            "Height" -> 1
                        |>
                    },
                    {
                        <|
                            "Components" -> {
                                Repeated[
                                    <|"Shape" -> <|"Name" -> "Pixel"|>, "Image" -> "Same"|>,
                                    {2}
                                ]
                            }
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
            "Image" -> Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, -1, -1}, {-1, -1, 1}}],
            "Position" -> {1, 1},
            "Type" -> "Group"
        |>,
        <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {1, 1}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {3, 3}|>
    }
    ,
    TestID -> "ARCFormGroupsWhenApplyingRules-20220827-Z84ACQ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`SimplifyObjects["ExtraKeys" -> {"Type"}][
            Daniel`ARC`ARCFormGroupsWhenApplyingRules[
                {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{1}}],
                        "PixelPositions" -> {{1, 1}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Colors" -> {1},
                        "Color" -> 1,
                        "Position" -> {1, 1},
                        "Y" -> 1,
                        "X" -> 1,
                        "Width" -> 1,
                        "Height" -> 1
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "PixelPositions" -> {{3, 3}},
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Colors" -> {2},
                        "Color" -> 2,
                        "Position" -> {3, 3},
                        "Y" -> 3,
                        "X" -> 3,
                        "Width" -> 1,
                        "Height" -> 1
                    |>
                },
                {
                    <|
                        "Components" -> {
                            Repeated[
                                <|"Shape" -> <|"Name" -> "Pixel"|>, "Image" -> "Same"|>,
                                {2}
                            ]
                        }
                    |>
                },
                10,
                10
            ]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {1, 1}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{2}}], "Position" -> {3, 3}|>
    }
    ,
    TestID -> "ARCFormGroupsWhenApplyingRules-20220827-8VG4ZO"
]