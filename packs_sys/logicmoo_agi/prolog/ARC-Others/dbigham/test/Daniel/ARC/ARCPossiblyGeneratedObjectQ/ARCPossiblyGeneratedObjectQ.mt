(*
    Tests for: Daniel`ARC`ARCPossiblyGeneratedObjectQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPossiblyGeneratedObjectQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCPossiblyGeneratedObjectQ[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]
                ][[
                    "Objects",
                    1
                ]]
            ]
        ]
    ]
    ,
    <|
        "Result" -> True,
        "TurnCount" -> 6,
        "Line" -> {
            {1, 1},
            {1, 2},
            {1, 3},
            {1, 4},
            {1, 5},
            {1, 6},
            {2, 6},
            {3, 6},
            {4, 6},
            {5, 6},
            {6, 6},
            {6, 5},
            {6, 4},
            {6, 3},
            {6, 2},
            {6, 1},
            {5, 1},
            {4, 1},
            {3, 1},
            {3, 2},
            {3, 3},
            {3, 4},
            {4, 4},
            {4, 3}
        }
    |>
    ,
    TestID -> "ARCPossiblyGeneratedObjectQ-20220914-4VG3KZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCPossiblyGeneratedObjectQ[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]
                ][[
                    "Objects",
                    1
                ]],
                "FirstPosition" -> {1, 3}
            ]
        ]
    ]
    ,
    <|
        "Result" -> True,
        "TurnCount" -> 6,
        "Line" -> {
            {1, 1},
            {1, 2},
            {1, 3},
            {1, 4},
            {1, 5},
            {1, 6},
            {2, 6},
            {3, 6},
            {4, 6},
            {5, 6},
            {6, 6},
            {6, 5},
            {6, 4},
            {6, 3},
            {6, 2},
            {6, 1},
            {5, 1},
            {4, 1},
            {3, 1},
            {3, 2},
            {3, 3},
            {3, 4},
            {4, 4},
            {4, 3}
        }
    |>
    ,
    TestID -> "ARCPossiblyGeneratedObjectQ-20220914-I7TJWQ"
]

Test[
    Daniel`ARC`ARCPossiblyGeneratedObjectQ[
        Daniel`ARC`ARCParseScene[
            Daniel`ARC`ARCParseFile["e5790162"]["Train", 1, "Output"],
            "FormMultiColorCompositeObjects" -> False
        ][[
            "Objects",
            1
        ]]
    ]
    ,
    <|
        "Result" -> True,
        "TurnCount" -> 1,
        "Line" -> {{3, 1}, {3, 2}, {3, 3}, {4, 3}, {5, 3}, {6, 3}}
    |>
    ,
    TestID -> "ARCPossiblyGeneratedObjectQ-20220921-GFL67M"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCPossiblyGeneratedObjectQ[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["d9f24cd1"]["Train", 2, "Output"],
                    "FormMultiColorCompositeObjects" -> False,
                    "CheckForGridsAndDividers" -> False
                ][[
                    "Objects",
                    1
                ]]
            ]
        ]
    ]
    ,
    <|
        "Result" -> True,
        "TurnCount" -> 2,
        "Line" -> {
            {10, 2},
            {9, 2},
            {8, 2},
            {7, 2},
            {7, 3},
            {6, 3},
            {5, 3},
            {4, 3},
            {3, 3},
            {2, 3},
            {1, 3}
        }
    |>
    ,
    TestID -> "ARCPossiblyGeneratedObjectQ-20220925-K15TTB"
]

Test[
    Daniel`ARC`ARCPossiblyGeneratedObjectQ[
        Append[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["d9f24cd1"]["Train", 1, "Output"],
                "FormMultiColorCompositeObjects" -> False,
                "CheckForGridsAndDividers" -> False
            ][[
                "Objects",
                1
            ]],
            "Input" -> <|"Position" -> {10, 2}, "Width" -> 1, "Height" -> 1|>
        ]
    ]
    ,
    <|
        "Result" -> True,
        "TurnCount" -> 0,
        "Line" -> {
            {10, 2},
            {9, 2},
            {8, 2},
            {7, 2},
            {6, 2},
            {5, 2},
            {4, 2},
            {3, 2},
            {2, 2},
            {1, 2}
        }
    |>
    ,
    TestID -> "ARCPossiblyGeneratedObjectQ-20220925-YWTDZO"
]