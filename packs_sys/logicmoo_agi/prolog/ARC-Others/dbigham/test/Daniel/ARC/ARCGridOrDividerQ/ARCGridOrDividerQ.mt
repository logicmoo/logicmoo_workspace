(*
    Tests for: Daniel`ARC`ARCGridOrDividerQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGridOrDividerQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[
            {
                {-1, -1, 5, -1, -1, 5, -1, -1},
                {-1, -1, 5, -1, -1, 5, -1, -1},
                {5, 5, 5, 5, 5, 5, 5, 5},
                {-1, -1, 5, -1, -1, 5, -1, -1},
                {-1, -1, 5, -1, -1, 5, -1, -1},
                {5, 5, 5, 5, 5, 5, 5, 5},
                {-1, -1, 5, -1, -1, 5, -1, -1},
                {-1, -1, 5, -1, -1, 5, -1, -1}
            },
            1,
            1,
            8,
            8
        ]
    ]
    ,
    <|"Type" -> "Grid"|>
    ,
    TestID -> "ARCGridOrDividerQ-20220820-RQ5QYR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[{{5}, {5}, {5}, {5}}, 1, 3, 5, 4]
    ]
    ,
    <|"Type" -> "Divider", "Orientation" -> "Vertical"|>
    ,
    TestID -> "ARCGridOrDividerQ-20220820-ORUTJS"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[{{5, 5, 5, 5, 5}}, 3, 1, 5, 4]
    ]
    ,
    <|"Type" -> "Divider", "Orientation" -> "Horizontal"|>
    ,
    TestID -> "ARCGridOrDividerQ-20220820-BZLAT3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[{{5}, {5}, {5}}, 1, 3, 5, 4]
    ]
    ,
    False
    ,
    TestID -> "ARCGridOrDividerQ-20220820-85JJ4T"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[{{5, 5, 5, 5}}, 3, 1, 5, 4]
    ]
    ,
    False
    ,
    TestID -> "ARCGridOrDividerQ-20220820-Y2TG05"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCGridOrDividerQ[
                "Objects" -> Daniel`ARC`ARCImageRegionToObject[
                    Utility`ReturnIfFailure[
                        Daniel`ARC`ARCContiguousImageRegions[
                            Daniel`ARC`ARCParseFile["363442ee"][[1, "Train", 1, "Input"]],
                            "Background" -> 0
                        ]
                    ],
                    13,
                    9
                ],
                13,
                9
            ][[
                All,
                "GridOrDivider"
            ]]
        ]
    ]
    ,
    {
        Missing["KeyAbsent", "GridOrDivider"],
        Missing["KeyAbsent", "GridOrDivider"],
        Missing["KeyAbsent", "GridOrDivider"],
        Missing["KeyAbsent", "GridOrDivider"],
        Missing["KeyAbsent", "GridOrDivider"],
        Missing["KeyAbsent", "GridOrDivider"],
        <|
            "Type" -> "Divider",
            "Orientation" -> "Vertical",
            "RowCount" -> 1,
            "ColumnCount" -> 2,
            "Color" -> 5,
            "Cells" -> {
                {
                    <|"Y" -> 1, "X" -> 1, "Width" -> 3, "Height" -> 9|>,
                    <|"Y" -> 1, "X" -> 5, "Width" -> 9, "Height" -> 9|>
                }
            }
        |>,
        Missing["KeyAbsent", "GridOrDivider"]
    }
    ,
    TestID -> "ARCGridOrDividerQ-20220820-82AR2Q"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGridOrDividerQ[{{5}, {5}, {5}, {5}}, 1, 1, 5, 4]
    ]
    ,
    False
    ,
    TestID -> "ARCGridOrDividerQ-20220821-DACML8"
]

Test[
    Daniel`ARC`ARCGridOrDividerQ[{{5, 5, 5, 5, 5}}, 1, 1, 5, 4]
    ,
    False
    ,
    TestID -> "ARCGridOrDividerQ-20220821-RHMMV3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCGridOrDividerQ[
                "Objects" -> Daniel`ARC`ARCImageRegionToObject[
                    Utility`ReturnIfFailure[
                        Daniel`ARC`ARCContiguousImageRegions[
                            Daniel`ARC`ARCParseFile["272f95fa"][[1, "Train", 1, "Input"]],
                            "Background" -> 0
                        ]
                    ],
                    13,
                    9
                ],
                19,
                18
            ][[
                All,
                "GridOrDivider"
            ]]
        ]
    ]
    ,
    {
        <|
            "Type" -> "Grid",
            "RowCount" -> 3,
            "ColumnCount" -> 3,
            "Color" -> 8,
            "Cells" -> {
                {
                    <|"Y" -> 1, "X" -> 1, "Width" -> 4, "Height" -> 2|>,
                    <|"Y" -> 1, "X" -> 6, "Width" -> 6, "Height" -> 2|>,
                    <|"Y" -> 1, "X" -> 13, "Width" -> 7, "Height" -> 2|>
                },
                {
                    <|"Y" -> 4, "X" -> 1, "Width" -> 4, "Height" -> 4|>,
                    <|"Y" -> 4, "X" -> 6, "Width" -> 6, "Height" -> 4|>,
                    <|"Y" -> 4, "X" -> 13, "Width" -> 7, "Height" -> 4|>
                },
                {
                    <|"Y" -> 9, "X" -> 1, "Width" -> 4, "Height" -> 10|>,
                    <|"Y" -> 9, "X" -> 6, "Width" -> 6, "Height" -> 10|>,
                    <|"Y" -> 9, "X" -> 13, "Width" -> 7, "Height" -> 10|>
                }
            }
        |>
    }
    ,
    TestID -> "ARCGridOrDividerQ-20220910-8PMBAH"
]