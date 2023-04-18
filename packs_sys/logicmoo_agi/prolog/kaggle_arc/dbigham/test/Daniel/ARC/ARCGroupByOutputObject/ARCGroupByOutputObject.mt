(*
    Tests for: Daniel`ARC`ARCGroupByOutputObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGroupByOutputObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Function[
                Replace[
                    #1,
                    assoc_Association :> ERPTesting`NormalizeOutput[assoc],
                    {1, Infinity},
                    Heads -> True
                ]
            ][
                Normal[
                    Daniel`ARC`ARCGroupByOutputObject[
                        <|
                            <|
                                "UUID" -> 1,
                                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                                "PixelPositions" -> {{4, 3}},
                                "Colors" -> {8},
                                "Position" -> {4, 3},
                                "Width" -> 1,
                                "Height" -> 1,
                                "Length" -> 1,
                                "Y" -> 4,
                                "X" -> 3
                            |> -> <|
                                "UUID" -> 3
                            |>,
                            <|
                                "UUID" -> 2,
                                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                                "PixelPositions" -> {{4, 10}},
                                "Colors" -> {8},
                                "Position" -> {4, 10},
                                "Width" -> 1,
                                "Height" -> 1,
                                "Length" -> 1,
                                "Y" -> 4,
                                "X" -> 10
                            |> -> <|
                                "UUID" -> 3
                            |>,
                            <|
                                "UUID" -> 4,
                                "Image" -> Daniel`ARC`ARCScene[{{8}}],
                                "PixelPositions" -> {{6, 6}},
                                "Colors" -> {8},
                                "Position" -> {6, 6},
                                "Width" -> 1,
                                "Height" -> 1,
                                "Length" -> 1,
                                "Y" -> 6,
                                "X" -> 6
                            |> -> <|
                                "UUID" -> 5
                            |>
                        |>,
                        <|"Width" -> 10, "Height" -> 10|>,
                        <|
                            "Width" -> 10,
                            "Height" -> 10,
                            "Objects" -> {
                                <|"UUID" -> 3, "MyKey" -> 1|>,
                                <|"UUID" -> 5, "MyKey" -> 2|>
                            }
                        |>
                    ]
                ]
            ]
        ]
    ]
    ,
    {
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
                    "Colors" -> {8},
                    "Position" -> {4, 3},
                    "Width" -> 1,
                    "Height" -> 1,
                    "Length" -> 1,
                    "Y" -> 4,
                    "X" -> 3
                |>,
                <|
                    "UUID" -> 0,
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "PixelPositions" -> {{4, 10}},
                    "Colors" -> {8},
                    "Position" -> {4, 10},
                    "Width" -> 1,
                    "Height" -> 1,
                    "Length" -> 1,
                    "Y" -> 4,
                    "X" -> 10
                |>
            },
            "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, -1, -1, -1, -1, -1, -1, 10}}],
            "Color" -> 8,
            "ColorCount" -> 1,
            "MostUsedColor" -> 8,
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
            "HorizontalLineSymmetry" -> True,
            "VerticalAndHorizontalLineSymmetry" -> True,
            "HollowCount" -> 0
        |> -> <|
            "UUID" -> 0,
            "MyKey" -> 1
        |>,
        <|
            "UUID" -> 0,
            "Image" -> Daniel`ARC`ARCScene[{{8}}],
            "PixelPositions" -> {{6, 6}},
            "Colors" -> {8},
            "Position" -> {6, 6},
            "Width" -> 1,
            "Height" -> 1,
            "Length" -> 1,
            "Y" -> 6,
            "X" -> 6
        |> -> <|
            "UUID" -> 0
        |>
    }
    ,
    TestID -> "ARCGroupByOutputObject-20220805-OLUF8Y"
]