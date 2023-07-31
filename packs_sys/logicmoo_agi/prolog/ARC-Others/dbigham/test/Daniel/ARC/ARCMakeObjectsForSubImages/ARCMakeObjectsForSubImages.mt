(*
    Tests for: Daniel`ARC`ARCMakeObjectsForSubImages
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMakeObjectsForSubImages]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCMakeObjectsForSubImages[
                        <|
                            "UUID" -> "f3922cf7-30b2-4899-afb2-2d67e0ab35bd",
                            "Image" -> Daniel`ARC`ARCScene[
                                {
                                    {5, 1, 5, -1, -1, -1, -1, -1, -1},
                                    {1, -1, 1, -1, -1, -1, -1, -1, -1},
                                    {5, 1, 5, 5, 1, 5, -1, -1, -1},
                                    {-1, -1, -1, 1, -1, 1, -1, -1, -1},
                                    {-1, -1, -1, 5, 1, 5, 5, 1, 5},
                                    {-1, -1, -1, -1, -1, -1, 1, -1, 1},
                                    {-1, -1, -1, -1, -1, -1, 5, 1, 5}
                                }
                            ],
                            "PixelPositions" -> {
                                {1, 1},
                                {1, 2},
                                {1, 3},
                                {2, 1},
                                {2, 3},
                                {3, 1},
                                {3, 2},
                                {3, 3},
                                {3, 4},
                                {3, 5},
                                {3, 6},
                                {4, 4},
                                {4, 6},
                                {5, 4},
                                {5, 5},
                                {5, 6},
                                {5, 7},
                                {5, 8},
                                {5, 9},
                                {6, 7},
                                {6, 9},
                                {7, 7},
                                {7, 8},
                                {7, 9}
                            },
                            "Colors" -> {1, 5},
                            "Width" -> 9,
                            "Height" -> 7,
                            "Position" -> {1, 1}
                        |>,
                        {
                            <|
                                "Image" -> {{5, 1, 5}, {1, -1, 1}, {5, 1, 5}},
                                "Count" -> 1,
                                "ExampleObjects" -> {
                                    <|
                                        "UUID" -> "4ed18af1-ee40-41d9-ad04-c9a0693199e4",
                                        "Image" -> Daniel`ARC`ARCScene[
                                            {{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}
                                        ],
                                        "PixelPositions" -> {
                                            {7, 2},
                                            {7, 3},
                                            {7, 4},
                                            {8, 2},
                                            {8, 4},
                                            {9, 2},
                                            {9, 3},
                                            {9, 4}
                                        },
                                        "Shapes" -> {
                                            <|
                                                "Image" -> Daniel`ARC`ARCScene[
                                                    {{5, 1, 5}, {1, -1, 1}, {5, 1, 5}}
                                                ]
                                            |>
                                        },
                                        "Colors" -> {1, 5},
                                        "Width" -> 3,
                                        "Height" -> 3,
                                        "Position" -> {7, 2},
                                        "Y" -> 7,
                                        "X" -> 2,
                                        "AspectRatio" -> 1,
                                        "Area" -> 9,
                                        "FilledArea" -> 8,
                                        "Components" -> {
                                            <|
                                                "Image" -> Daniel`ARC`ARCScene[{{1}}],
                                                "Position" -> {7, 2},
                                                "PixelPositions" -> {{7, 2}}
                                            |>
                                        }
                                    |>
                                }
                            |>
                        },
                        Daniel`ARC`ARCScene[
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
                        ],
                        0
                    ]
                ]
            ]
        ]
    ]
    ,
    {
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
        |>
    }
    ,
    TestID -> "ARCMakeObjectsForSubImages-20220725-G2VBC1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        ERPTesting`NormalizeOutput[
            Daniel`ARC`ARCMakeObjectsForSubImages[
                <|
                    "UUID" -> "f3922cf7-30b2-4899-afb2-2d67e0ab35bd",
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1}, {1, 1, 1, 1}}],
                    "PixelPositions" -> {
                        {1, 1},
                        {1, 2},
                        {1, 3},
                        {1, 4},
                        {2, 1},
                        {2, 2},
                        {2, 3},
                        {2, 4}
                    },
                    "Colors" -> {1},
                    "Position" -> {1, 1}
                |>,
                {
                    <|
                        "Image" -> {{1, 1}, {1, 1}},
                        "Count" -> 1,
                        "ExampleObjects" -> {
                            <|
                                "UUID" -> "4ed18af1-ee40-41d9-ad04-c9a0693199e4",
                                "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
                                "PixelPositions" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
                                "Colors" -> {1},
                                "Position" -> {7, 2}
                            |>
                        }
                    |>
                },
                Daniel`ARC`ARCScene[{{0, 0, 0}, {0, 0, 0}}],
                0
            ]
        ]
    ]
    ,
    <|
        "UUID" -> 0,
        "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1}, {1, 1, 1, 1}}],
        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {1, 4}, {2, 1}, {2, 2}, {2, 3}, {2, 4}},
        "Colors" -> {1},
        "Position" -> {1, 1}
    |>
    ,
    TestID -> "ARCMakeObjectsForSubImages-20220725-EAY17F"
]