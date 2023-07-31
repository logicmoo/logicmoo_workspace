(*
    Tests for: Daniel`ARC`ARCFixOcclusionsForSubImages
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFixOcclusionsForSubImages]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                "ExtraKeys" -> {"Y", "Y2", "X", "X2", "Width", "Height", "Area", "Components"}
            ][
                Module[
                    {
                        parsedScene = Daniel`ARC`ARCParseScene[
                            Daniel`ARC`ARCParseFile["d364b489"]["Train"][[1, "Output"]],
                            "FindOcclusions" -> False
                        ]
                    },
                    Daniel`ARC`ARCFixOcclusionsForSubImages[
                        Daniel`ARC`ARCSubImagesForOcclusionDetection[
                            {
                                <|
                                    "Image" -> {{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}},
                                    "Count" -> 5
                                |>
                            }
                        ],
                        parsedScene
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {1, 6},
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 6,
            "X2" -> 8,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {1, 7},
                    "Y" -> 1,
                    "Y2" -> 1,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {2, 6},
                    "Y" -> 2,
                    "Y2" -> 2,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {2, 7},
                    "Y" -> 2,
                    "Y2" -> 2,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {2, 8},
                    "Y" -> 2,
                    "Y2" -> 2,
                    "X" -> 8,
                    "X2" -> 8,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {3, 7},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {3, 9},
            "Y" -> 3,
            "Y2" -> 5,
            "X" -> 9,
            "X2" -> 11,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {3, 10},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {4, 9},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 9,
                    "X2" -> 9,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {4, 10},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {4, 11},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 11,
                    "X2" -> 11,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {5, 10},
                    "Y" -> 5,
                    "Y2" -> 5,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {5, 3},
            "Y" -> 5,
            "Y2" -> 7,
            "X" -> 3,
            "X2" -> 5,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {5, 4},
                    "Y" -> 5,
                    "Y2" -> 5,
                    "X" -> 4,
                    "X2" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {6, 3},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 3,
                    "X2" -> 3,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {6, 4},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 4,
                    "X2" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {6, 5},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 5,
                    "X2" -> 5,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {7, 4},
                    "Y" -> 7,
                    "Y2" -> 7,
                    "X" -> 4,
                    "X2" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {7, 7},
            "Y" -> 7,
            "Y2" -> 9,
            "X" -> 7,
            "X2" -> 9,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {7, 8},
                    "Y" -> 7,
                    "Y2" -> 7,
                    "X" -> 8,
                    "X2" -> 8,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {8, 7},
                    "Y" -> 8,
                    "Y2" -> 8,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {8, 8},
                    "Y" -> 8,
                    "Y2" -> 8,
                    "X" -> 8,
                    "X2" -> 8,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {8, 9},
                    "Y" -> 8,
                    "Y2" -> 8,
                    "X" -> 9,
                    "X2" -> 9,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {9, 8},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 8,
                    "X2" -> 8,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {9, 1},
            "Y" -> 9,
            "Y2" -> 11,
            "X" -> 1,
            "X2" -> 3,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {9, 2},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 2,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {10, 1},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 1,
                    "X2" -> 1,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {10, 2},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 2,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {10, 3},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 3,
                    "X2" -> 3,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {11, 2},
                    "Y" -> 11,
                    "Y2" -> 11,
                    "X" -> 2,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>
    }
    ,
    TestID -> "ARCFixOcclusionsForSubImages-20221023-KWAH2C"
]

Test[
    Daniel`ARC`SimplifyObjects[
        "ExtraKeys" -> {"Y", "Y2", "X", "X2", "Width", "Height", "Area", "Components"}
    ][
        Module[
            {
                parsedScene = Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["d364b489"]["Train"][[2, "Output"]],
                    "FindOcclusions" -> False
                ]
            },
            Daniel`ARC`ARCFixOcclusionsForSubImages[
                Daniel`ARC`ARCSubImagesForOcclusionDetection[
                    {<|"Image" -> {{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}, "Count" -> 5|>}
                ],
                parsedScene
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {0, 5},
            "Y" -> 0,
            "Y2" -> 2,
            "X" -> 5,
            "X2" -> 7,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {0, 6},
                    "Y" -> 0,
                    "Y2" -> 0,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {1, 5},
                    "Y" -> 1,
                    "Y2" -> 1,
                    "X" -> 5,
                    "X2" -> 5,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {1, 6},
                    "Y" -> 1,
                    "Y2" -> 1,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {1, 7},
                    "Y" -> 1,
                    "Y2" -> 1,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {2, 6},
                    "Y" -> 2,
                    "Y2" -> 2,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {2, 0},
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 0,
            "X2" -> 2,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {2, 1},
                    "Y" -> 2,
                    "Y2" -> 2,
                    "X" -> 1,
                    "X2" -> 1,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {3, 0},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 0,
                    "X2" -> 0,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {3, 1},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 1,
                    "X2" -> 1,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {3, 2},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 2,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {4, 1},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 1,
                    "X2" -> 1,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {3, 9},
            "Y" -> 3,
            "Y2" -> 5,
            "X" -> 9,
            "X2" -> 11,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {3, 10},
                    "Y" -> 3,
                    "Y2" -> 3,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {4, 9},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 9,
                    "X2" -> 9,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {4, 10},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {4, 11},
                    "Y" -> 4,
                    "Y2" -> 4,
                    "X" -> 11,
                    "X2" -> 11,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {5, 10},
                    "Y" -> 5,
                    "Y2" -> 5,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {5, 5},
            "Y" -> 5,
            "Y2" -> 7,
            "X" -> 5,
            "X2" -> 7,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {5, 6},
                    "Y" -> 5,
                    "Y2" -> 5,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {6, 5},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 5,
                    "X2" -> 5,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {6, 6},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {6, 7},
                    "Y" -> 6,
                    "Y2" -> 6,
                    "X" -> 7,
                    "X2" -> 7,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {7, 6},
                    "Y" -> 7,
                    "Y2" -> 7,
                    "X" -> 6,
                    "X2" -> 6,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {8, 2},
            "Y" -> 8,
            "Y2" -> 10,
            "X" -> 2,
            "X2" -> 4,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {8, 3},
                    "Y" -> 8,
                    "Y2" -> 8,
                    "X" -> 3,
                    "X2" -> 3,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {9, 2},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 2,
                    "X2" -> 2,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {9, 3},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 3,
                    "X2" -> 3,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {9, 4},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 4,
                    "X2" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {10, 3},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 3,
                    "X2" -> 3,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Position" -> {9, 9},
            "Y" -> 9,
            "Y2" -> 11,
            "X" -> 9,
            "X2" -> 11,
            "Width" -> 3,
            "Height" -> 3,
            "Area" -> 9,
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> {9, 10},
                    "Y" -> 9,
                    "Y2" -> 9,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{7}}],
                    "Position" -> {10, 9},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 9,
                    "X2" -> 9,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                    "Position" -> {10, 10},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{6}}],
                    "Position" -> {10, 11},
                    "Y" -> 10,
                    "Y2" -> 10,
                    "X" -> 11,
                    "X2" -> 11,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{8}}],
                    "Position" -> {11, 10},
                    "Y" -> 11,
                    "Y2" -> 11,
                    "X" -> 10,
                    "X2" -> 10,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Area" -> 1
                |>
            }
        |>
    }
    ,
    TestID -> "ARCFixOcclusionsForSubImages-20221023-6130CI"
]