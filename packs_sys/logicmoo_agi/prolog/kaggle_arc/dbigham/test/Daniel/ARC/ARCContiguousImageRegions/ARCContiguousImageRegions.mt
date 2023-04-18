(*
    Tests for: Daniel`ARC`ARCContiguousImageRegions
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCContiguousImageRegions]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousImageRegions[{{2, 2, 2}, {2, 1, 8}, {2, 8, 8}}]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 1,
                "Position" -> {2, 2},
                "Image" -> {{1}},
                "PixelPositions" -> {{2, 2}}
            |>,
            <|
                "Color" -> 2,
                "Position" -> {1, 1},
                "Image" -> {{2, 2, 2}, {2, -1, -1}, {2, -1, -1}},
                "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}}
            |>,
            <|
                "Color" -> 8,
                "Position" -> {2, 2},
                "Image" -> {{-1, 8}, {8, 8}},
                "PixelPositions" -> {{2, 3}, {3, 2}, {3, 3}}
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220717-WKCEJQ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousImageRegions[{{1, 1, 1}, {8, 1, 3}, {8, 2, 2}}]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 1,
                "Position" -> {1, 1},
                "Image" -> {{1, 1, 1}, {-1, 1, -1}},
                "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 2}}
            |>,
            <|
                "Color" -> 2,
                "Position" -> {3, 2},
                "Image" -> {{2, 2}},
                "PixelPositions" -> {{3, 2}, {3, 3}}
            |>,
            <|
                "Color" -> 3,
                "Position" -> {2, 3},
                "Image" -> {{3}},
                "PixelPositions" -> {{2, 3}}
            |>,
            <|
                "Color" -> 8,
                "Position" -> {2, 1},
                "Image" -> {{8}, {8}},
                "PixelPositions" -> {{2, 1}, {3, 1}}
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220717-RNMSEP"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousImageRegions[{{2, 0, 0}, {0, 2, 0}, {0, 0, 2}}]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 2,
                "Position" -> {1, 1},
                "Image" -> {{2, -1, -1}, {-1, 2, -1}, {-1, -1, 2}},
                "PixelPositions" -> {{1, 1}, {2, 2}, {3, 3}}
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220717-DS5F8I"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousImageRegions[
            {{2, 0, 0}, {0, 2, 0}, {0, 0, 2}},
            "FollowDiagonals" -> False
        ]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 2,
                "Position" -> {1, 1},
                "Image" -> {{2}},
                "PixelPositions" -> {{1, 1}}
            |>,
            <|
                "Color" -> 2,
                "Position" -> {2, 2},
                "Image" -> {{2}},
                "PixelPositions" -> {{2, 2}}
            |>,
            <|
                "Color" -> 2,
                "Position" -> {3, 3},
                "Image" -> {{2}},
                "PixelPositions" -> {{3, 3}}
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220717-6PZ6CY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCContiguousImageRegions[
                Replace[
                    Daniel`ARC`ARCParseFile["b60334d2"]["Train", 1, "Output"][[1]],
                    Except[0, _Integer] :> 10,
                    {2}
                ]
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 10,
                "Position" -> {2, 3},
                "Image" -> {
                    {10, 10, 10, -1, -1, -1},
                    {10, -1, 10, -1, -1, -1},
                    {10, 10, 10, -1, -1, -1},
                    {-1, -1, -1, 10, 10, 10},
                    {-1, -1, -1, 10, -1, 10},
                    {-1, -1, -1, 10, 10, 10}
                },
                "PixelPositions" -> {
                    {2, 3},
                    {2, 4},
                    {2, 5},
                    {3, 3},
                    {3, 5},
                    {4, 3},
                    {4, 4},
                    {4, 5},
                    {5, 6},
                    {5, 7},
                    {5, 8},
                    {6, 6},
                    {6, 8},
                    {7, 6},
                    {7, 7},
                    {7, 8}
                }
            |>,
            <|
                "Color" -> 10,
                "Position" -> {7, 2},
                "Image" -> {{10, 10, 10}, {10, -1, 10}, {10, 10, 10}},
                "PixelPositions" -> {
                    {7, 2},
                    {7, 3},
                    {7, 4},
                    {8, 2},
                    {8, 4},
                    {9, 2},
                    {9, 3},
                    {9, 4}
                }
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220725-L81EQJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousImageRegions[
            {{0, 1, 1}, {1, 0, 1}, {1, 1, 0}},
            "Background" -> 1
        ]
    ]
    ,
    Daniel`ARC`ARCImageRegions[
        {
            <|
                "Color" -> 0,
                "Position" -> {1, 1},
                "Image" -> {{0, -1, -1}, {-1, 0, -1}, {-1, -1, 0}},
                "PixelPositions" -> {{1, 1}, {2, 2}, {3, 3}}
            |>
        }
    ]
    ,
    TestID -> "ARCContiguousImageRegions-20220806-MTBH9U"
]