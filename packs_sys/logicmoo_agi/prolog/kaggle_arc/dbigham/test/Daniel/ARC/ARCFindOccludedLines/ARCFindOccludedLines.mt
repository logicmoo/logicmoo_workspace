(*
    Tests for: Daniel`ARC`ARCFindOccludedLines
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindOccludedLines]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Daniel`ARC`ARCParseScene[
                            Daniel`ARC`ARCParseFile["178fcbfb"]["Train", 1, "Output"],
                            "FormMultiColorCompositeObjects" -> False
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}}],
            "Position" -> {1, 3},
            "ZOrder" -> 1
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1, 1, 1, 1, 1, 1}}],
            "Position" -> {7, 1},
            "ZOrder" -> 0
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{3, 3, 3, 3, 3, 3, 3, 3, 3}}],
            "Position" -> {5, 1},
            "ZOrder" -> 0
        |>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220826-DJASJJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Utility`PrintIfFailure[
                            Daniel`ARC`ARCParseScene[
                                Daniel`ARC`ARCParseFile["1A2E2828"]["Train", 1, "Input"],
                                "FormMultiColorCompositeObjects" -> False
                            ]
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{2, 2}, {2, 2}, {2, 2}, {2, 2}, {2, 2}, {2, 2}, {2, 2}, {2, 2}}
            ],
            "Position" -> {1, 2},
            "ZOrder" -> 1
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, {3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}}
            ],
            "Position" -> {3, 1},
            "ZOrder" -> 2
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{8}, {8}, {8}, {8}, {8}, {8}, {8}, {8}}],
            "Position" -> {1, 8},
            "ZOrder" -> 1
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}}],
            "Position" -> {6, 1},
            "ZOrder" -> 0
        |>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-HZCF8D"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Utility`PrintIfFailure[
                            Daniel`ARC`ARCParseScene[
                                Daniel`ARC`ARCParseFile["1A2E2828"]["Train", 3, "Input"],
                                "FormMultiColorCompositeObjects" -> False
                            ]
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
                }
            ],
            "Position" -> {2, 1},
            "ZOrder" -> 2
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2},
                    {2, 2}
                }
            ],
            "Position" -> {1, 2},
            "ZOrder" -> 2
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}}
            ],
            "Position" -> {7, 1},
            "ZOrder" -> 1
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8},
                    {8, 8}
                }
            ],
            "Position" -> {1, 8},
            "ZOrder" -> 2
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{6}, {6}, {6}, {6}, {6}, {6}, {6}, {6}, {6}, {6}, {6}}
            ],
            "Position" -> {1, 5},
            "ZOrder" -> 0
        |>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-A5QY5X"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Utility`PrintIfFailure[
                            Daniel`ARC`ARCParseScene[
                                Daniel`ARC`ARCParseFile["1A2E2828"]["Train", 5, "Input"],
                                "FormMultiColorCompositeObjects" -> False
                            ]
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}, {1}, {1}}],
            "Position" -> {1, 2},
            "ZOrder" -> 1
        |>,
        <|"Image" -> Daniel`ARC`ARCScene[{{3, 3, 3}}], "Position" -> {2, 1}, "ZOrder" -> 0|>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-R57FFR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Utility`PrintIfFailure[
                            Daniel`ARC`ARCParseScene[
                                Daniel`ARC`ARCParseFile["746b3537"]["Train", 1, "Output"],
                                "FormMultiColorCompositeObjects" -> False
                            ]
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{2}}], "Position" -> {2, 1}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {1, 1}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {3, 1}, "ZOrder" -> 0|>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-TFW9R4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
                With[
                    {
                        scene = Utility`PrintIfFailure[
                            Daniel`ARC`ARCParseScene[
                                Daniel`ARC`ARCParseFile["253bf280"]["Train", 1, "Output"],
                                "FormMultiColorCompositeObjects" -> False
                            ]
                        ]
                    },
                    Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{3, 3, 3, 3, 3, 3}}],
            "Position" -> {4, 4},
            "ZOrder" -> 0
        |>,
        <|"Image" -> Daniel`ARC`ARCScene[{{8}}], "Position" -> {4, 3}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{8}}], "Position" -> {4, 10}, "ZOrder" -> 0|>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-W6RA8O"
]

Test[
    Daniel`ARC`SimplifyObjects["ExtraKeys" -> "ZOrder"][
        With[
            {
                scene = Utility`PrintIfFailure[
                    Daniel`ARC`ARCParseScene[
                        Daniel`ARC`ARCParseFile["25d8a9c8"]["Train", 3, "Input"],
                        "FormMultiColorCompositeObjects" -> False
                    ]
                ]
            },
            Daniel`ARC`ARCFindOccludedLines[scene["Scene"], 0, scene["Objects"]]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}}], "Position" -> {2, 1}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{2}}], "Position" -> {1, 1}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{2}}], "Position" -> {1, 3}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{9}}], "Position" -> {1, 2}, "ZOrder" -> 0|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{9, 9, 9}}], "Position" -> {3, 1}, "ZOrder" -> 0|>
    }
    ,
    TestID -> "ARCFindOccludedLines-20220829-Q1PSSK"
]