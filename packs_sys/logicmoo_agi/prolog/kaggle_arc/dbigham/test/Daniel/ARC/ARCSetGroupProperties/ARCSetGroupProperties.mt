(*
    Tests for: Daniel`ARC`ARCSetGroupProperties
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSetGroupProperties]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            ERPTesting`NormalizeOutput[
                Daniel`ARC`ARCSetGroupProperties[
                    {
                        <|
                            "UUID" -> "a79ffda8-e340-456f-a62b-3e7e09dba6a7",
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "PixelPositions" -> {{4, 3}},
                            "Shapes" -> {
                                <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|>,
                                <|"Name" -> "Pixel"|>,
                                <|"Name" -> "Square"|>,
                                <|"Name" -> "Rectangle"|>
                            },
                            "Colors" -> {8},
                            "Position" -> {4, 3},
                            "Width" -> 1,
                            "Height" -> 1,
                            "Length" -> 1,
                            "Y" -> 4,
                            "X" -> 3,
                            "AspectRatio" -> 1,
                            "Area" -> 1,
                            "FilledArea" -> 1,
                            "WidthRank" -> 1,
                            "HeightRank" -> 1,
                            "LengthRank" -> 1,
                            "YRank" -> 1,
                            "XRank" -> 2,
                            "AspectRatioRank" -> 1
                        |>,
                        <|
                            "UUID" -> "e63c5889-828b-4fc8-bdec-a0433dfaf7c1",
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "PixelPositions" -> {{4, 10}},
                            "Shapes" -> {
                                <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|>,
                                <|"Name" -> "Pixel"|>,
                                <|"Name" -> "Square"|>,
                                <|"Name" -> "Rectangle"|>
                            },
                            "Colors" -> {8},
                            "Position" -> {4, 10},
                            "Width" -> 1,
                            "Height" -> 1,
                            "Length" -> 1,
                            "Y" -> 4,
                            "X" -> 10,
                            "AspectRatio" -> 1,
                            "Area" -> 1,
                            "FilledArea" -> 1,
                            "WidthRank" -> 1,
                            "HeightRank" -> 1,
                            "LengthRank" -> 1,
                            "YRank" -> 1,
                            "XRank" -> 1,
                            "AspectRatioRank" -> 1
                        |>
                    },
                    10,
                    10
                ]
            ]
        ]
    ]
    ,
    <|
        "Type" -> "Group",
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{8, -1, -1, -1, -1, -1, -1, 8}}],
        "PixelPositions" -> {{4, 3}, {4, 10}},
        "Colors" -> {8},
        "Position" -> {4, 3},
        "Y" -> 4,
        "X" -> 3,
        "Y2" -> 4,
        "X2" -> 10,
        "Width" -> 8,
        "Height" -> 1,
        "Components" -> {
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                "PixelPositions" -> {{4, 3}},
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|>,
                    <|"Name" -> "Pixel"|>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>
                },
                "Colors" -> {8},
                "Position" -> {4, 3},
                "Width" -> 1,
                "Height" -> 1,
                "Length" -> 1,
                "Y" -> 4,
                "X" -> 3,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "FilledArea" -> 1,
                "WidthRank" -> 1,
                "HeightRank" -> 1,
                "LengthRank" -> 1,
                "YRank" -> 1,
                "XRank" -> 2,
                "AspectRatioRank" -> 1
            |>,
            <|
                "UUID" -> 0,
                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                "PixelPositions" -> {{4, 10}},
                "Shapes" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|>,
                    <|"Name" -> "Pixel"|>,
                    <|"Name" -> "Square"|>,
                    <|"Name" -> "Rectangle"|>
                },
                "Colors" -> {8},
                "Position" -> {4, 10},
                "Width" -> 1,
                "Height" -> 1,
                "Length" -> 1,
                "Y" -> 4,
                "X" -> 10,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "FilledArea" -> 1,
                "WidthRank" -> 1,
                "HeightRank" -> 1,
                "LengthRank" -> 1,
                "YRank" -> 1,
                "XRank" -> 1,
                "AspectRatioRank" -> 1
            |>
        },
        "Color" -> 8,
        "ColorCount" -> 1,
        "YInverse" -> 7,
        "XInverse" -> 8,
        "Y2Inverse" -> 7,
        "X2Inverse" -> 1,
        "ZOrder" -> 0,
        "YMiddle" -> 4,
        "Length" -> 8,
        "PrimarySizeDimension" -> "X",
        "AspectRatio" -> 8,
        "Area" -> 8,
        "FilledArea" -> 2,
        "FilledProportion" -> 0.25,
        "SurfacePixelCount" -> 2,
        "VerticalLineSymmetry" -> True,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCSetGroupProperties-20220805-EO5LHW"
]