(*
    Tests for: Daniel`ARC`ARCSubImagesForOcclusionDetection
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubImagesForOcclusionDetection]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubImagesForOcclusionDetection[
            {
                <|"Image" -> {{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}, "Count" -> 5|>,
                <|"Image" -> {{2, -1}, {1, 6}, {8, -1}}, "Count" -> 1|>,
                <|"Image" -> {{-1, 2}, {7, 1}, {-1, 8}}, "Count" -> 2|>,
                <|"Image" -> {{7, 1, 6}, {-1, 8, -1}}, "Count" -> 1|>,
                <|"Image" -> {{-1, 2, -1}, {7, 1, 6}}, "Count" -> 1|>,
                <|"Image" -> {{-1, 2}, {7, 1}}, "Count" -> 1|>
            }
        ]
    ]
    ,
    <|
        Daniel`ARC`ARCScene[{{-1, 2}, {7, 1}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{-1, 2}, {7, 1}, {-1, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{7, 1}, {-1, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 2,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{7, 1, 6}, {-1, 8, -1}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 2,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{2, -1}, {1, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 2,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{2, -1}, {1, 6}, {8, -1}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 2,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{1, 6}, {8, -1}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {7, 1, 6}, {-1, 8, -1}}],
            "Y" -> 2,
            "Y2" -> 3,
            "X" -> 2,
            "X2" -> 3
        |>
    |>
    ,
    TestID -> "ARCSubImagesForOcclusionDetection-20221023-3086J8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubImagesForOcclusionDetection[
            {
                <|
                    "Image" -> {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}},
                    "Count" -> 5
                |>,
                Null
            }
        ]
    ]
    ,
    <|
        Daniel`ARC`ARCScene[{{1, 2, 3}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 1,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3, 4}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 1,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{1, 2}, {5, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3}, {5, 6, 7}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3, 4}, {5, 6, 7, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{1}, {5}, {9}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 1
        |>,
        Daniel`ARC`ARCScene[{{1, 2}, {5, 6}, {9, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3}, {5, 6, 7}, {9, 8, 7}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{1}, {5}, {9}, {5}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 1
        |>,
        Daniel`ARC`ARCScene[{{1, 2}, {5, 6}, {9, 8}, {5, 4}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{1, 2, 3}, {5, 6, 7}, {9, 8, 7}, {5, 4, 3}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{5, 4, 3}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 4,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{5, 4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 4,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{9, 8}, {5, 4}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 3,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{9, 8, 7}, {5, 4, 3}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 3,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{9, 8, 7, 6}, {5, 4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 3,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{5}, {9}, {5}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 1
        |>,
        Daniel`ARC`ARCScene[{{5, 6}, {9, 8}, {5, 4}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 2
        |>,
        Daniel`ARC`ARCScene[{{5, 6, 7}, {9, 8, 7}, {5, 4, 3}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 3
        |>,
        Daniel`ARC`ARCScene[{{5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 1,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{2, 3, 4}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 1,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{3, 4}, {7, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 3,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{2, 3, 4}, {6, 7, 8}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 2,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{4}, {8}, {6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 4,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{3, 4}, {7, 8}, {7, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 3,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{2, 3, 4}, {6, 7, 8}, {8, 7, 6}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 3,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{4}, {8}, {6}, {2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 4,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{3, 4}, {7, 8}, {7, 6}, {3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 3,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{2, 3, 4}, {6, 7, 8}, {8, 7, 6}, {4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 1,
            "Y2" -> 4,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 4,
            "Y2" -> 4,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{7, 6}, {3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 3,
            "Y2" -> 4,
            "X" -> 3,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{8, 7, 6}, {4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 3,
            "Y2" -> 4,
            "X" -> 2,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{8}, {6}, {2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 4,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{7, 8}, {7, 6}, {3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 3,
            "X2" -> 4
        |>,
        Daniel`ARC`ARCScene[{{6, 7, 8}, {8, 7, 6}, {4, 3, 2}}] -> <|
            "FullImage" -> Daniel`ARC`ARCScene[
                {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 8, 7, 6}, {5, 4, 3, 2}}
            ],
            "Y" -> 2,
            "Y2" -> 4,
            "X" -> 2,
            "X2" -> 4
        |>
    |>
    ,
    TestID -> "ARCSubImagesForOcclusionDetection-20221023-CNUYYV"
]